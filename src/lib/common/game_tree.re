open! Core;

module GS = Game_state;
module PS = Player_state;

type t = {
  state: GS.t,
  mutable subtrees: option(list((Action.t, t))),
};

let create = state => {state, subtrees: None};

let get_state = t => t.state;

/** A helper function that computes all legal moves for [t]'s current player,
    and associates them with the resulting game tree. */

let compute_subtrees_with_moves = (t): list((Action.t, t)) => {
  let board = GS.get_board_minus_penguins(t.state)
  and current = GS.get_current_player(t.state);
  /* Return all legal positions a penguin can move to from [src] on [board] */
  let get_legal_move_dsts_from = (src: Position.t): list(Position.t) =>
    Board.get_reachable_from(board, src)
    |> List.map(~f=((_, dsts)) => dsts)
    |> List.concat;
  open List.Let_syntax;
  let next_gs = GS.rotate_to_next_player(t.state); /* player always rotates */
  let%bind src =
    PS.get_penguins(current) |> List.map(~f=Penguin.get_position);
  let%bind dst = get_legal_move_dsts_from(src);
  let moved_next_gs = GS.move_penguin(next_gs, src, dst);
  let act = Action.Move({src, dst});
  return((act, {state: moved_next_gs, subtrees: None}));
};

/** In general, the subtrees of [t] has the form:
      t --skip--> t1 --skip--> ... --skip--> tk --moves--> ...
    or it has no subtrees if no player can move.
    EFFECT: Update the [subtrees] field from [t] to [tk] to "stitch them up"
    If it didn't generate up to a moveable subtree, [t.subtree] is [Some[]] */

let generate_until_moveable_subtree = (t): unit => {
  let start_color = GS.get_current_player(t.state) |> PS.get_color;
  /* if _next_ (not current) player in [t] has color [start_color], stop. */
  let rec generate_to_moveable_tree_until_start = (t): option(t) =>
    switch (compute_subtrees_with_moves(t)) {
    | [_, ..._] as subtrees =>
      t.subtrees = Some(subtrees);
      Some(t);
    | [] =>
      open Option.Let_syntax; /* current player in [t] can't move, skip it */

      let next_gs = GS.rotate_to_next_player(t.state);
      let next_color = GS.get_current_player(next_gs) |> PS.get_color;
      let next_t = {...t, state: next_gs};
      let%bind subtree =
        if (PS.Player_color.equal(next_color, start_color)) {
          None;
        } else {
          /* no player was able to move */
          generate_to_moveable_tree_until_start(
            next_t,
          );
        };
      t.subtrees = Some([(Action.Skip, subtree)]);
      return(t);
    };
  switch (generate_to_moveable_tree_until_start(t)) {
  | None => t.subtrees = Some([])
  | _ => ()
  };
};

let get_subtrees = t =>
  switch (t.subtrees) {
  | Some(subtrees) => subtrees
  | None =>
    generate_until_moveable_subtree(t);
    switch (t.subtrees) {
    | None => failwith("subtrees of t must have been initialized")
    | Some(subtrees) => subtrees
    };
  };
