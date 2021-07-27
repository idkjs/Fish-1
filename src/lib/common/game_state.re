open! Core;

module Player_color = Player_state.Player_color;
module PS = Player_state;
module CQ = Util.Circular_queue;

/* For implementation convenience */
module Player_list = {
  /** A [t] represents state of all the players in a fish game.
      It's agnostic to the board (boundary, etc.)
      It ensures:
      - all players have distinct colors
      NOTE that it's immutable */

  type t = {players: list(PS.t)};

  /** Create a [t] with 1 player for each of the given colors. */

  let create = (colors: list(Player_color.t)): t =>
    if (List.contains_dup(~compare=Player_color.compare, colors)) {
      failwith("Colors in a fish game must be unique");
    } else {
      {players: List.map(~f=PS.create, colors)};
    };

  /** Return [None] if no player has [color] in [t] */

  let get_player_with_color = (t, color): option(PS.t) =>
    List.find(t.players, ~f=p => Player_color.equal(color, PS.get_color(p)));

  /** ERROR: if no player has [color] in [t] */

  let remove_player_with_color_exn = (t, color): t => {
    let rec remove_in_players = (players, checked: list(PS.t)): list(PS.t) =>
      switch (players) {
      | [] => failwith("No player has given color in the player list")
      | [p, ...players] =>
        if (Player_color.equal(color, PS.get_color(p))) {
          checked @ players;
        } else {
          remove_in_players(players, [p, ...checked]);
        }
      };

    {players: remove_in_players(t.players, [])};
  };

  let any_player_has_penguin_at = (t, pos: Position.t): bool => {
    let player_has_penguin_at_pos = (p: PS.t): bool =>
      PS.get_penguins(p)
      |> List.map(~f=Penguin.get_position)
      |> List.exists(~f=[%compare.equal: Position.t](pos));

    List.exists(t.players, ~f=player_has_penguin_at_pos);
  };

  /** Move the penguin at [src] to [dst]. The integer represents the # of fish
      on the tile at 1st position  Update player score based on this # of fish.
      Errors if no penguin is at [src], or a penguin exists at [dst] */

  let move_penguin = (t, src: Position.t, dst: Position.t, fish: int): t => {
    let rec update_players = players =>
      switch (players) {
      | [] => failwith("No penguin resides at source position")
      | [p, ...players] =>
        switch (PS.move_penguin(p, src, dst)) {
        | None => [p, ...update_players(players)]
        | Some(p) =>
          let new_score = fish + PS.get_score(p);
          [PS.set_score(p, new_score), ...players];
        }
      };

    if (any_player_has_penguin_at(t, dst)) {
      failwith("Cannot move penguin to a tile occupied by another penguin");
    } else {
      {players: update_players(t.players)};
    };
  };

  /** Place a new penguin with given color at given position on the board.
      Errors if the no the participating player has given color */

  let place_penguin = (t, color: Player_color.t, pos: Position.t): t => {
    let penguin = Penguin.create(pos);
    let rec update_players = players =>
      switch (players) {
      | [] => failwith("No player has given color")
      | [p, ...players] =>
        if (Player_color.equal(color) @@ PS.get_color(p)) {
          [PS.add_penguin(p, penguin), ...players];
        } else {
          [p, ...update_players(players)];
        }
      };

    if (any_player_has_penguin_at(t, pos)) {
      failwith(
        "Cannot place penguin onto a tile occupied by another penguin",
      );
    } else {
      {players: update_players(t.players)};
    };
  };

  /** Discouraged unless you have good reason and know what you are doing */

  let from_players = (players: list(PS.t)): result(t, string) => {
    let colors = List.map(~f=PS.get_color, players);
    if (List.contains_dup(~compare=Player_color.compare, colors)) {
      Result.fail("Players must have distinct colors");
    } else {
      Result.return({players: players});
    };
  };
};

module PL = Player_list;

type t = {
  board: Board.t,
  players: PL.t,
  order: CQ.t(Player_color.t),
};

let create = (board, colors) =>
  switch (colors) {
  | [] => failwith("There must be at least 1 player in a game")
  | [start, ...nexts] => {
      board,
      players: PL.create(colors),
      order: CQ.create(start, nexts),
    }
  };

let get_board_copy = t => Board.get_copy(t.board);

let get_ordered_players = t => {
  let opt_players =
    CQ.to_list(t.order)
    |> List.map(~f=PL.get_player_with_color(t.players))
    |> Option.all;
  switch (opt_players) {
  | None => failwith("Some color(s) are missing in player list")
  | Some(players) => players
  };
};

let get_current_player = t =>
  switch (PL.get_player_with_color(t.players) @@ CQ.get_current(t.order)) {
  | None => failwith("Current player color is missing in player list")
  | Some(player) => player
  };

let rotate_to_next_player = t => {...t, order: CQ.rotate(t.order)};

let remove_current_player = t => {
  let current_color = CQ.get_current(t.order);
  open Option.Let_syntax;
  let%bind order = CQ.remove_current(t.order);
  let players = PL.remove_player_with_color_exn(t.players, current_color);
  return({...t, players, order});
};

let get_player_with_color = (t, color) =>
  switch (PL.get_player_with_color(t.players, color)) {
  | None => failwith("No player has specified color in this game state")
  | Some(p) => p
  };

let get_board_minus_penguins = t => {
  let board = ref @@ Board.get_copy(t.board);
  let remove_penguin_tiles_of_player = (p: PS.t): unit =>
    PS.get_penguins(p)
    |> List.iter(~f=pg =>
         board := Board.remove_tile_at(board^, Penguin.get_position(pg))
       );

  get_ordered_players(t) |> List.iter(~f=remove_penguin_tiles_of_player);
  board^;
};

let place_penguin = (t, color, pos) => {
  if (Tile.is_hole @@ Board.get_tile_at(t.board, pos)) {
    failwith("Cannot place penguin onto a hole");
  };
  {...t, players: PL.place_penguin(t.players, color, pos)};
};

let move_penguin = (t, src, dst) => {
  if (Tile.is_hole @@ Board.get_tile_at(t.board, dst)) {
    failwith("Cannot move a penguin onto a hole");
  };
  let fish = Board.get_tile_at(t.board, src) |> Tile.get_fish;
  let players = PL.move_penguin(t.players, src, dst, fish);
  let board = Board.remove_tile_at(Board.get_copy(t.board), src);
  {...t, board, players};
};

let from_board_players = (board, players) =>
  switch (players) {
  | [] => Result.fail("There must be at least 1 player in a game")
  | [start, ...nexts] =>
    open Result.Let_syntax;
    let%bind player_list = PL.from_players(players);
    let penguin_positions =
      List.concat_map(~f=PS.get_penguins, players)
      |> List.map(~f=Penguin.get_position);
    if (List.contains_dup(~compare=Position.compare, penguin_positions)) {
      Result.fail("Each tile must have at most 1 penguin");
    } else if (List.exists(
                 ~f=Fun.negate @@ Board.within_board(board),
                 penguin_positions,
               )) {
      Result.fail("All penguins must reside on tiles withint the board");
    } else if (List.map(~f=Board.get_tile_at(board), penguin_positions)
               |> List.exists(~f=Tile.is_hole)) {
      Result.fail("No penguin should reside on a hole");
    } else {
      let start = PS.get_color(start);
      let nexts = List.map(~f=PS.get_color, nexts);
      let order = CQ.create(start, nexts);
      return({board, players: player_list, order});
    };
  };
