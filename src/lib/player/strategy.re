open! Core;

module PS = Player_state;
module GS = Game_state;
module GT = Game_tree;
module Dir = Board.Direction;

module Penguin_placer = {
  type t =
    | Scanning;

  let rec leftmost_non_hole_pos_from =
          (b: Board.t, from: Position.t): option(Position.t) =>
    if ((!) @@ Board.within_board(b, from)) {
      None;
    } else if ((!) @@ Tile.is_hole(Board.get_tile_at(b, from))) {
      Some(from);
    } else {
      let from_se = Dir.step_from(from, Dir.Southeast);
      let from_right = Dir.step_from(from_se, Dir.Northeast);
      leftmost_non_hole_pos_from(b, from_right);
    };

  let rec topleft_most_non_hole_pos_from =
          (b: Board.t, from: Position.t): option(Position.t) =>
    if ((!) @@ Board.within_board(b, from)) {
      None;
    } else {
      /* Check 2 rows each time, navigate via directions. */
      let row1_res = leftmost_non_hole_pos_from(b, from);
      let row2_start = Dir.step_from(from, Dir.Southeast);
      let row2_res = leftmost_non_hole_pos_from(b, row2_start);
      switch (Option.first_some(row1_res, row2_res)) {
      | None =>
        topleft_most_non_hole_pos_from(b, Dir.step_from(from, Dir.South))
      | res => res
      };
    };

  let use_scanning = (gs: GS.t): Position.t => {
    let board = GS.get_board_minus_penguins(gs);
    let top_left_pos = Board.get_top_left_pos(board);
    let best_pos = topleft_most_non_hole_pos_from(board, top_left_pos);
    switch (best_pos) {
    | None => failwith("No position to place penguin on board")
    | Some(pos) => pos
    };
  };

  let create_scanning_strategy = Scanning;

  let use = (t, gs) =>
    switch (t) {
    | Scanning => use_scanning(gs)
    };
};

module Turn_actor = {
  type t =
    /* positive # of turns a player should take while looking ahead */
    | Minimax(int);

  /* This can break ties in moves with same score */
  let act_compare = (act1: Action.t, act2: Action.t): int =>
    switch (act1, act2) {
    /* favor top-left move */
    | (Action.Move(m1), Action.Move(m2)) => - Action.Move.compare(m1, m2)
    | _ => 0
    }; /* treat all other types of action pairs as "equal" */

  let scored_act_compare = ((act1, score1), (act2, score2)): int =>
    if (score1 == score2) {
      act_compare(act1, act2);
    } else {
      Int.compare(score1, score2);
    };

  let use_minimax = (turns, gt) => {
    let my_color = gt |> GT.get_state |> GS.get_current_player |> PS.get_color;
    let current_player_is_me = (gt: GT.t): bool =>
      GT.get_state(gt)
      |> GS.get_current_player
      |> PS.get_color
      |> PS.Player_color.equal(my_color);

    let get_my_player_score = (gt: GT.t): int =>
      PS.get_score @@ GS.get_player_with_color(GT.get_state(gt), my_color);

    /* Evaluate [gt] from the perspective of player with [color], by looking
     * ahead as many steps as needed so that the player takes at least
     * [turns_left] more turns. Evaluate based on the minimax algorithm and
     * player score */
    let rec evaluate_tree = (turns_left: int, gt: GT.t): int =>
      if (turns_left == 0) {
        get_my_player_score(gt);
      } else {
        let (score_selector, next_turns_left) =
          if (current_player_is_me(gt)) {
            (List.max_elt(~compare=Int.compare), turns_left - 1);
          } else {
            (List.min_elt(~compare=Int.compare), turns_left);
          };
        let scores =
          List.map(GT.get_subtrees(gt), ~f=((_, gt)) =>
            evaluate_tree(next_turns_left, gt)
          );
        switch (score_selector(scores)) {
        /* [None] means end of game */
        | None => get_my_player_score(gt)
        | Some(score) => score
        };
      };

    let best_scored_move =
      GT.get_subtrees(gt)
      |> List.map(~f=((act, subt)) =>
           (act, evaluate_tree(turns - 1, subt))
         )
      |> List.max_elt(~compare=scored_act_compare);
    switch (best_scored_move) {
    | None => failwith("No legal action in given game state")
    | Some((act, _)) => act
    };
  };

  let create_minimax_strategy = turns => Minimax(turns);

  let use = (t, gt) =>
    switch (t) {
    | Minimax(turns) => use_minimax(turns, gt)
    };
};
