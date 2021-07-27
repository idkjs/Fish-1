open! Core;

type t = {tiles: array(array(Tile.t))};

module Direction = {
  type t =
    | North
    | South
    | Northeast
    | Southeast
    | Northwest
    | Southwest;

  /** Return the position of the adjust tile to ([row], [col])
      in direction [dir] */

  let step_in_dir = (row: int, col: int, dir: t): (int, int) => {
    let even_row = row mod 2 == 0;
    switch (dir) {
    | North => (row - 2, col)
    | South => (row + 2, col)
    | Northeast => (row - 1, col + (if (even_row) {0} else {1}))
    | Southeast => (row + 1, col + (if (even_row) {0} else {1}))
    | Northwest => (row - 1, col + (if (even_row) {(-1)} else {0}))
    | Southwest => (row + 1, col + (if (even_row) {(-1)} else {0}))
    };
  };

  let values = [North, South, Northeast, Southeast, Northwest, Southwest];

  let step_from = ({Position.row, col}, dir) => {
    let (row, col) = step_in_dir(row, col, dir);
    {Position.row, col};
  };
};

module Config = {
  type t = {
    width: int,
    height: int,
    holes: list(Position.t),
    min_one_fish_tile: int,
    default_num_of_fish: int,
  };

  let create = (~height, ~width) => {
    width,
    height,
    holes: [],
    min_one_fish_tile: 0,
    default_num_of_fish: 1,
  };

  let set_width = (width, t) => {...t, width};
  let get_width = t => t.width;

  let set_height = (height, t) => {...t, height};
  let get_height = t => t.height;

  let set_holes = (holes, t) => {...t, holes};
  let get_holes = t => t.holes;

  let set_min_num_of_one_fish_tile = (min_one_fish_tile, t) => {
    ...t,
    min_one_fish_tile,
  };
  let get_min_num_of_one_fish_tile = t => t.min_one_fish_tile;

  let set_default_num_of_fish = (default_num_of_fish, t) => {
    ...t,
    default_num_of_fish,
  };
  let get_default_num_of_fish = t => t.default_num_of_fish;
};

module C = Config;

let create = config => {
  let (width, height) = (
    Config.get_width(config),
    Config.get_height(config),
  );
  if (width <= 0 || height <= 0) {
    failwith("Board dimension must be positive");
  };
  let holes = Config.get_holes(config);
  let dft_fish = Config.get_default_num_of_fish(config);
  let one_fish_tiles_left =
    ref @@ Config.get_min_num_of_one_fish_tile(config);
  let default_tile = Tile.create(dft_fish);
  let tiles = Array.make_matrix(~dimy=width, ~dimx=height, default_tile);
  holes |> List.iter(~f=({Position.row, col}) => tiles[row][col] = Tile.hole);
  Position.create_positions_within(~height, ~width)
  |> List.iter(~f=({Position.row, col}) =>
       if ((!) @@ Tile.is_hole(tiles[row][col]) && one_fish_tiles_left^ > 0) {
         tiles[row][col] = Tile.create(1);
         one_fish_tiles_left := one_fish_tiles_left^ - 1;
       }
     );
  {tiles: tiles};
};

let get_width = t => Array.length(t.tiles[0]);

let get_height = t => Array.length(t.tiles);

let get_top_left_pos = _ => {Position.row: 0, col: 0};

let within_board = (t, {Position.row, col}) => {
  let (width, height) = (get_width(t), get_height(t));
  0 <= row && row < height && 0 <= col && col < width;
};

let get_tile_at = (t, {Position.row, col} as pos) =>
  if (within_board(t, pos)) {
    t.tiles[row][col];
  } else {
    failwith("Position is outside the board");
  };

let remove_tile_at = (t, {Position.row, col} as pos) => {
  if (within_board(t, pos)) {
    t.tiles[row][col] = Tile.hole;
  } else {
    failwith("Position is outside the board");
  };
  t;
};

let num_of_non_hole_tiles = t => {
  let (width, height) = (get_width(t), get_height(t));
  Position.create_positions_within(~width, ~height)
  |> List.filter(~f=pos => (!) @@ Tile.is_hole @@ get_tile_at(t, pos))
  |> List.length;
};

let get_reachable_from = (t, src) => {
  /* From (row, col), attempt to add AMAP position to `acc` following `dir`
   * Assume (row, col) has been considered. */
  let rec add_until_cant = (row, col, dir, acc) => {
    let (row, col) = Direction.step_in_dir(row, col, dir);
    let pos = {Position.row, col};
    if (within_board(t, pos) && (!) @@ Tile.is_hole @@ t.tiles[row][col]) {
      add_until_cant(row, col, dir, [pos, ...acc]);
    } else {
      List.rev(acc);
    };
  };

  if ((!) @@ within_board(t, src)) {
    [];
  } else {
    List.map(Direction.values, ~f=dir =>
      (dir, add_until_cant(src.row, src.col, dir, []))
    );
  };
};

let get_copy = t => {tiles: Array.map(~f=Array.copy, t.tiles)};

let from_tiles = tiles => {
  let height = List.length(tiles);
  if (height == 0) {
    Result.fail("There should be at least 1 row");
  } else {
    switch (
      List.map(~f=List.length, tiles) |> List.max_elt(~compare=Int.compare)
    ) {
    | None => Result.fail("At least 1 row should be non-empty")
    | Some(width) =>
      let arr = Array.make_matrix(~dimx=height, ~dimy=width, Tile.hole);
      List.iteri(tiles, ~f=(row, tiles) =>
        List.iteri(tiles, ~f=(col, tile) => arr[row][col] = tile)
      );
      Result.return({tiles: arr});
    };
  };
};
