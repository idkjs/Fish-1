module GS = Common.Game_state;
module BD = Common.Board;
module PC = Common.Player_state.Player_color;
module PS = Common.Player_state;
module Tile = Common.Tile;
module Penguin = Common.Penguin;
module Pos = Util.Position;

module Constants = {
  let tile_size = 30; /* In terms of pixels */
  let window_title = "Fish!";
  let fish_color = Graphics.black;
  let tile_boundary_color = Graphics.black;
  let hole_tile_color = Graphics.rgb(200, 200, 200);
  let occupied_tile_color = Graphics.rgb(255, 165, 0);
};
/* -------------------------------------------------------------- */
/* ----------------------- helper functions --------------------- */
/* -------------------------------------------------------------- */

/** For the hexagon located at (row, col), return the unscaled (row, col) of
    the topleft point of the smallest rectangle enclosing the hexagon. */

let get_topleft_boundary_pos = (row: int, col: int, height: int): (int, int) => {
  let colOffset =
    if (row mod 2 === 1) {
      2;
    } else {
      0;
    };
  let row = height - row - 1; /* [Graphics] has lower left corner as (0, 0) */
  (row, colOffset + 4 * col);
};

/** A hexagon tile is rendered as
      A------B
     /        \
    F  fish... C
     \        /
      E------D
    Where the coordiates relative to top-left point of bounding rectangle are:
    A = (0, 1), B = (0, 2), C = (1, 3), D = (2, 2), E = (2, 1), F = (1, 0)
    where each coordinate is (row, col), and normalized by [tile_size] */

let render_tile = (tile: Tile.t, {Pos.row, col}: Pos.t, height: int): unit => {
  let (dr, dc) = get_topleft_boundary_pos(row, col, height);
  open Constants;
  /* draw tile */
  let vertices =
    Array.map(
      ((r, c)) => ((c + dc) * tile_size, (r + dr) * tile_size),
      [|(0, 1), (0, 2), (1, 3), (2, 2), (2, 1), (1, 0)|],
    );

  Graphics.set_color(tile_boundary_color);
  Graphics.draw_poly(vertices);
  if (Tile.is_hole(tile)) {
    Graphics.set_color(hole_tile_color);
  } else {
    Graphics.set_color(occupied_tile_color);
  };
  Graphics.fill_poly(vertices);
  /* draw fish */
  Graphics.moveto((dc + 1) * tile_size, (dr + 1) * tile_size);
  Graphics.set_color(fish_color);
  Graphics.draw_string(string_of_int @@ Tile.get_fish(tile));
};

/** Render [penguin] at its position with [color] */

let render_penguin = (penguin: Penguin.t, color: PC.t, height: int): unit => {
  let {Pos.row, col} = Penguin.get_position(penguin);
  let (row, col) = get_topleft_boundary_pos(row, col, height);
  let c =
    switch (color) {
    | White => Graphics.white
    | Red => Graphics.red
    | Black => Graphics.black
    | Brown => Graphics.rgb(210, 105, 30)
    };
  open Constants;
  Graphics.set_color(c);
  let x = (col + 1) * tile_size + tile_size / 2;
  let y = (row + 1) * tile_size;
  let r = tile_size / 2;
  Graphics.fill_circle(x, y, r);
};

/** Render relavent information associated with [player] */

let render_player = (player: PS.t, height: int): unit => {
  let color = PS.get_color(player);
  PS.get_penguins(player)
  |> List.iter(penguin => render_penguin(penguin, color, height));
};

/** Render the entire board of hexagon tiles */

let render_board = (board: BD.t): unit => {
  let (height, width) = (BD.get_height(board), BD.get_width(board));
  Pos.create_positions_within(~width, ~height)
  |> List.iter(pos => render_tile(BD.get_tile_at(board, pos), pos, height));
};

/** Resize the window based on game state */

let resize_window = (gs: GS.t): unit => {
  let board = GS.get_board_copy(gs);
  let (height, width) = (BD.get_height(board), BD.get_width(board));
  let min_height = (1 + height) * Constants.tile_size;
  let min_width = (1 + 4 * width) * Constants.tile_size;
  Graphics.resize_window(min_width, min_height);
};

/* -------------------------------------------------------------- */
/* ---------------------- exported functions -------------------- */
/* -------------------------------------------------------------- */
let render = {
  Printexc.record_backtrace(true);
  let already_init = ref(false);
  gs => {
    let init = () => {
      Graphics.open_graph("");
      Graphics.set_window_title(Constants.window_title);
      Graphics.resize_window(1, 1);
      Graphics.auto_synchronize(false);
      already_init := true;
    };

    if ((!) @@ already_init^) {
      init();
    };
    resize_window(gs);
    render_board @@ GS.get_board_copy(gs);
    let height = gs |> GS.get_board_copy |> BD.get_height;
    gs |> GS.get_ordered_players |> List.iter(p => render_player(p, height));
    Graphics.display_mode(true);
    Graphics.synchronize();
  };
};
