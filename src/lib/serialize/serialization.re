open! Core;

module YB = Yojson.Basic;
module GS = Game_state;
module Player_color = Player_state.Player_color;

type t = YB.t;

/* -------------------------------------------------------------- */
/* ----------------------- helper functions --------------------- */
/* -------------------------------------------------------------- */
let rec assoc_res =
        (kvs: list((string, 'a)), key: string): result('a, string) =>
  switch (kvs) {
  | [(k, value), ..._] when String.equal(k, key) => Result.return(value)
  | [_, ...kvs] => assoc_res(kvs, key)
  | _ => Result.fail @@ "key [" ++ key ++ "] not found"
  };

let to_nat = (t: t, errmsg: string): result(int, string) =>
  switch (t) {
  | `Int(n) when n >= 0 => Result.return(n)
  | _ => Result.fail(errmsg)
  };

let from_pos = (pos: Position.t): t =>
  `List([`Int(pos.row), `Int(pos.col)]);

let to_pos = (t: t): result(Position.t, string) =>
  switch (t) {
  | `List([`Int(row), `Int(col)]) => Result.return({Position.row, col})
  | _ => Result.fail("Position expects an array of 2 integers")
  };

let from_tile = (tile: Tile.t): t => `Int(Tile.get_fish(tile));

let to_tile = (t: t): result(Tile.t, string) =>
  Result.bind(
    ~f=
      n =>
        Result.return(
          if (n == 0) {
            Tile.hole;
          } else {
            Tile.create(n);
          },
        ),
    to_nat(t, "Tile expects a single non-negative integer"),
  );

let from_board = (board: Board.t): t => {
  let tile_ts_2d = {
    open List.Let_syntax;
    let%bind row = List.init(Board.get_height(board), ~f=Fun.id);
    let row_t = {
      let%bind col = List.init(Board.get_width(board), ~f=Fun.id);
      [from_tile @@ Board.get_tile_at(board, {Position.row, col})];
    };
    [row_t];
  };

  `List(List.map(~f=tile_ts => `List(tile_ts), tile_ts_2d));
};

let to_board = (t: t): result(Board.t, string) => {
  let to_row = (t): result(list(Tile.t), string) =>
    switch (t) {
    | `List(tile_ts) => List.map(~f=to_tile, tile_ts) |> Result.all
    | _ => Result.fail("A row in board expects a list of tiles")
    };

  switch (t) {
  | `List(rows) =>
    List.map(~f=to_row, rows)
    |> Result.all
    |> Result.bind(~f=Board.from_tiles)
  | _ => Result.fail("A board expects a list of rows")
  };
};

let from_penguin = (penguin: Penguin.t): t =>
  from_pos @@ Penguin.get_position(penguin);

let to_penguin = (t: t): result(Penguin.t, string) =>
  Result.map(~f=Penguin.create) @@ to_pos(t);

let to_score = (t): result(int, string) =>
  to_nat(t, "Score must be a non-negative integer");

let from_color = (color: Player_color.t): t =>
  switch (color) {
  | Red => `String("red")
  | White => `String("white")
  | Black => `String("black")
  | Brown => `String("brown")
  };

let to_color = (t): result(Player_color.t, string) =>
  switch (t) {
  | `String("red") => Result.return(Player_color.Red)
  | `String("white") => Result.return(Player_color.White)
  | `String("black") => Result.return(Player_color.Black)
  | `String("brown") => Result.return(Player_color.Brown)
  | _ => Result.fail @@ "Unrecognized color: " ++ YB.to_string(t)
  };

let from_player = (player: Player_state.t): t => {
  let color_t = from_color @@ Player_state.get_color(player)
  and score_t = `Int(Player_state.get_score(player))
  and pengs_t =
    `List(List.map(~f=from_penguin) @@ Player_state.get_penguins(player));

  `Assoc([("color", color_t), ("score", score_t), ("places", pengs_t)]);
};

let to_player_state = (t: t): result(Player_state.t, string) =>
  Result.Let_syntax.(
    switch (t) {
    | `Assoc(kv_pairs) =>
      let%bind score_t = assoc_res(kv_pairs, "score")
      and color_t = assoc_res(kv_pairs, "color");
      switch%bind (assoc_res(kv_pairs, "places")) {
      | `List(pengs_t) =>
        let%bind pengs = List.map(~f=to_penguin, pengs_t) |> Result.all
        and score = to_score(score_t)
        and color = to_color(color_t);
        let player = Player_state.create(color);
        let player = Player_state.set_score(player, score);
        let player =
          List.fold_right(
            ~init=player,
            ~f=Fun.flip(Player_state.add_penguin),
            pengs,
          );
        return(player);
      | _ => Result.fail("Player expects a 'places' field")
      };
    | _ => Result.fail("Player needs to be a json object")
    }
  );

let to_player_list = (t: t): result(list(Player_state.t), string) =>
  switch (t) {
  | `List(player_ts) =>
    player_ts |> List.map(~f=to_player_state) |> Result.all
  | _ => Result.fail("Player list must be a list of players")
  };

let to_player = (t: t): result(Player.t, string) =>
  switch (t) {
  | `List([`String(name), `Int(depth)]) when 0 < depth && depth <= 2 =>
    Result.return @@
    Player.create_AI_player(
      ~name,
      Strategy.Penguin_placer.create_scanning_strategy,
      Strategy.Turn_actor.create_minimax_strategy(depth),
    )
  | _ => Result.fail("Player must be an array of name and search depth")
  };

let to_players = (t: t): result(list(Player.t), string) =>
  switch (t) {
  | `List(player_ts) => player_ts |> List.map(~f=to_player) |> Result.all
  | _ => Result.fail("Players must be an array of players")
  };

let from_action = act =>
  switch (act) {
  | Action.Skip => `String("skip")
  | Action.Move({src, dst}) => `List([from_pos(src), from_pos(dst)])
  };

let to_action = t =>
  switch (t) {
  | `String("skip") => Result.return(Action.Skip)
  | `List([src_t, dst_t]) =>
    open Result.Let_syntax;
    let%bind src = to_pos(src_t);
    let%bind dst = to_pos(dst_t);
    Result.return @@ Action.Move({src, dst});
  | _ => Result.fail("Action must be a skip string or move array")
  };

let from_board_posn = ((board, pos)) => {
  let bt = from_board(board);
  let pt = from_pos(pos);
  `Assoc([("board", bt), ("position", pt)]);
};

let to_board_posn = (t: t) =>
  Result.Let_syntax.(
    switch (t) {
    | `Assoc(kv_pairs) =>
      let%bind board_t = assoc_res(kv_pairs, "board")
      and pos_t = assoc_res(kv_pairs, "position");
      let%bind board = to_board(board_t)
      and pos = to_pos(pos_t);
      return((board, pos));
    | _ => Result.fail("Board_Posn must be a json object")
    }
  );

let from_game_state = gs => {
  let bt = from_board @@ GS.get_board_copy(gs)
  and plt = `List(List.map(~f=from_player) @@ GS.get_ordered_players(gs));

  `Assoc([("board", bt), ("players", plt)]);
};

let to_game_state = (t: t) =>
  Result.Let_syntax.(
    switch (t) {
    | `Assoc(kv_pairs) =>
      let%bind board_t = assoc_res(kv_pairs, "board")
      and players_t = assoc_res(kv_pairs, "players");
      let%bind board = to_board(board_t)
      and players = to_player_list(players_t);
      GS.from_board_players(board, players);
    | _ => Result.fail("Game state must be a json object")
    }
  );

let to_move_resp_query = t =>
  Result.Let_syntax.(
    switch (t) {
    | `Assoc(kv_pairs) =>
      let%bind state_t = assoc_res(kv_pairs, "state")
      and src_t = assoc_res(kv_pairs, "from")
      and dst_t = assoc_res(kv_pairs, "to");
      let%bind state = to_game_state(state_t)
      and src = to_pos(src_t)
      and dst = to_pos(dst_t);
      return((state, src, dst));
    | _ => Result.fail("Move_Response_Query must be a json object")
    }
  );

let to_game_description = t =>
  Result.Let_syntax.(
    switch (t) {
    | `Assoc(kv_pairs) =>
      let%bind players_t = assoc_res(kv_pairs, "players")
      and row_t = assoc_res(kv_pairs, "row")
      and col_t = assoc_res(kv_pairs, "column")
      and fish_t = assoc_res(kv_pairs, "fish");
      let%bind players = to_players(players_t)
      and row = to_nat(row_t, "row must be a natural number")
      and col = to_nat(col_t, "col must be a natural number")
      and fish = to_nat(fish_t, "default # of fish must be a natural number");
      return((row, col, players, fish));
    | _ => Result.fail("Move_Response_Query must be a json object")
    }
  );

let from_list = (xs, serializer) => `List(List.map(~f=serializer, xs));

let to_list = (t, deserializer) =>
  switch (t) {
  | `List(ts) => Result.return @@ List.map(~f=deserializer, ts)
  | _ => Result.fail("Expected an array of json values")
  };

/* Helper to catch type error during parsing */
let catch_type_error = (f: t => 'a, t: t): option('a) =>
  try(Some(f(t))) {
  | YB.Util.Type_error(_) => None
  };

let from_string = s => `String(s);

let to_string = t => catch_type_error(YB.Util.to_string, t);

let from_bool = b => `Bool(b);

let to_bool = t => catch_type_error(YB.Util.to_bool, t);

let from_int = b => `Int(b);

let to_int = t => catch_type_error(YB.Util.to_int, t);

let deserialize = str =>
  try(Some(YB.from_string(str))) {
  | Yojson.Json_error(_) => None
  };

let serialize = (t: t) => YB.to_string(t);

let stream_from_channel = in_chan => YB.stream_from_channel(in_chan);
