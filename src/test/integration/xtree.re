open! Core;
module S = Fish.Serialize.Serialization;
module B = Fish.Common.Board;
module Action = Fish.Common.Action;
module Move = Fish.Common.Action.Move;
module Game_tree = Fish.Common.Game_tree;
module GS = Fish.Common.Game_state;
module Pos = Fish.Util.Position;

let preference =
  B.Direction.[North, Northeast, Southeast, South, Southwest, Northwest];

/** Find the moves in [moves] that land in one of [dsts], prioritizing
    earlier elements in [dsts]. Return multiple moves if they all end on the
    same dst. */

let find_moves = (moves: list(Move.t), dsts: list(Pos.t)): list(Move.t) => {
  let rec go = (dsts: list(Pos.t)): list(Move.t) =>
    switch (dsts) {
    | [] => []
    | [d, ...dsts] =>
      switch (List.filter(~f=m => Pos.equal(d, m.dst), moves)) {
      | [] => go(dsts)
      | moves => moves
      }
    };
  go(dsts);
};

/** Return all the Moves within [actions], which might contain other types of
    actions. */

let rec get_moves_from_actions = (actions: list(Action.t)): list(Move.t) =>
  switch (actions) {
  | [] => []
  | [Action.Skip, ...rest] => get_moves_from_actions(rest)
  | [Action.Move(m), ...rest] => [m, ...get_moves_from_actions(rest)]
  };

/** apply the specified move, and find the move for next player that places a
    penguin onto a tile near previous move's destination, based on directional
    preference in [preference]. Break ties by selecting the top-leftmost move
    source. Output the selected move, or false if not possible */

let select_next_move_if_possible =
    (state: GS.t, src: Pos.t, dst: Pos.t): option(Move.t) => {
  open Option.Let_syntax;
  let subtrees = Game_tree.create(state) |> Game_tree.get_subtrees
  and act = Action.Move({src, dst});
  let%bind subtree = List.Assoc.find(~equal=Action.equal, subtrees, act);
  let moves =
    Game_tree.get_subtrees(subtree)
    |> List.map(~f=((move, _)) => move)
    |> get_moves_from_actions;
  let dsts = List.map(~f=B.Direction.step_from(dst), preference);
  let moves =
    find_moves(moves, dsts)  /* break ties here (if any) */
    |> List.sort(~compare=Action.Move.compare);
  List.hd(moves);
};

/** deserialize a move_response_query object, and print out the serialized form
    of selected move from [select_next_move_if_possible], or print "false" if
    desired move isn't possible. */

let () = {
  let input = Core.In_channel.input_all(Core.In_channel.stdin);
  let serialized =
    S.deserialize(input)
    |> Result.of_option(~error="invalid serialization form");
  switch (Result.bind(~f=S.to_move_resp_query, serialized)) {
  | Error(reason) => Printf.printf("Invalid input, reason: %s\n", reason)
  | [@implicit_arity] Ok(state, src, dst) =>
    switch (select_next_move_if_possible(state, src, dst)) {
    | None => print_string("false\n")
    | Some(m) =>
      S.from_action(Action.Move(m)) |> S.serialize |> Printf.printf("%s\n")
    }
  };
};
