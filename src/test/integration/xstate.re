open! Core;
module S = Fish.Serialize.Serialization;
module B = Fish.Common.Board;
module GS = Fish.Common.Game_state;
module PS = Fish.Common.Player_state;
module PN = Fish.Common.Penguin;
module Pos = Fish.Util.Position;

let preference =
  B.Direction.[North, Northeast, Southeast, South, Southwest, Northwest];

/** Find the closest move in [dir_pos], prioritizing earlier directions in
    [preference] */

let find_move =
    (dir_pos: list((B.Direction.t, list(Pos.t)))): option(Pos.t) => {
  let rec go = (prefs: list(B.Direction.t)): option(Pos.t) =>
    switch (prefs) {
    | [] => None
    | [p, ...prefs] =>
      switch (List.Assoc.find(~equal=phys_equal, dir_pos, p)) {
      | Some([pos, ..._]) => Some(pos)
      | _ => go(prefs)
      }
    };

  go(preference);
};

/** find a move based on [find_move] from the 1st penguin of the 1st player,
    then return game state resulted from applying that move to the original
    game state.  If such a move can't be found, return [None] */

let find_move_and_apply = (gs: GS.t): option(GS.t) => {
  open Option.Let_syntax;
  let players = GS.get_ordered_players(gs);
  let%bind player = List.hd(players);
  let%bind penguin = List.hd @@ PS.get_penguins(player);
  let src = PN.get_position(penguin);
  let board = GS.get_board_minus_penguins(gs);
  let%bind dst = find_move @@ B.get_reachable_from(board, src);
  return @@ GS.move_penguin(gs, src, dst);
};

/** deserialize a game state, and output the deserialized result of applying
    it to [find_move_and_apply]. If the result is [None], then print "false". */

let () = {
  let input = Core.In_channel.input_all(Core.In_channel.stdin);
  let serialized =
    S.deserialize(input)
    |> Result.of_option(~error="invalid serialization form");
  switch (Result.bind(~f=S.to_game_state, serialized)) {
  | Error(reason) => Printf.printf("Invalid input, reason: %s\n", reason)
  | Ok(state) =>
    switch (find_move_and_apply(state)) {
    | None => print_string("false\n")
    | Some(state) =>
      S.from_game_state(state) |> S.serialize |> Printf.printf("%s\n")
    }
  };
};
