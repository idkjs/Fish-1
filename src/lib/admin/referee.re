open! Core;
module Mutex = Error_checking_mutex;
module Timeout = Util.Timeout;

module Game_result = {
  type t = {
    winners: list(Player.t),
    cheaters: list(Player.t),
    failed: list(Player.t),
    rest: list(Player.t),
  };
};

module Game_observer = {
  type event =
    | Register(Game_state.t)
    | PenguinPlacement(Game_state.t, Player_state.Player_color.t, Position.t)
    | TurnAction(Game_state.t, Player_state.Player_color.t, Action.t)
    | Disqualify(option(Game_state.t), Player_state.Player_color.t)
    | EndOfGame(Game_result.t);
  type t = event => unit;
};

module Color = Common.Player_state.Player_color;
module GT = Common.Game_tree;
module GS = Common.Game_state;
module PS = Common.Player_state;

type timeout_config = {
  assign_color_timeout_ms: int,
  placement_timeout_ms: int,
  turn_action_timeout_ms: int,
  inform_disqualified_timeout_ms: int,
  inform_observer_timeout_ms: int,
};

type color_player_map = list((Color.t, Player.t));

/** A [t] represents a referee which manages an entire fish game from start to
    end. A [t] manages exactly 1 game and becomes obselete after the game ends.
    It's made mutable to enable client to do things such as query or add observers
    during a [run_game] call.
    It can:
    - Set up and run a game given an ordered list of [Player.t]s
    - Report final result of a game after it's finished */

type t = {
  /* current game state, updated during [run_game].
   * It's [None] before game starts or if all players have been removed. */
  mutable state: option(Game_state.t),
  /* mapping from color to player. Fixed at beginning of game even if players
   * get removed during the game */
  mutable color_to_player: color_player_map,
  mutable cheaters: list(Color.t),
  mutable failed: list(Color.t),
  mutable observers: list(Game_observer.t),
  conf: timeout_config,
  state_lock: Mutex.t,
};

/* Some constants */
module C = {
  let min_num_of_players = 2;
  let max_num_of_players = 4;
  let init_colors = [Color.Red, Color.Black, Color.White, Color.Brown];
};

/* synchronized read/write to the state */
let read_state = (t: t): option(GS.t) => {
  Mutex.lock(t.state_lock);
  let state = t.state;
  Mutex.unlock(t.state_lock);
  state;
};

let write_state = (t: t, state: option(GS.t)): unit => {
  Mutex.lock(t.state_lock);
  t.state = state;
  Mutex.unlock(t.state_lock);
};

/** EFFECT: update [t.observers] */

let add_game_observer = (t, observer) => {
  t.observers = [observer, ...t.observers];
  Option.iter(read_state(t), ~f=state => observer(Register(state)));
};

/** EFFECT: update [t.observers] to remove the observer(s) which time out */

let inform_all_observers = (t, event): unit => {
  let remaining_observers =
    List.filter(t.observers, ~f=observer =>
      Option.is_some @@
      Timeout.call_with_timeout_ms(
        () => observer(event),
        t.conf.inform_observer_timeout_ms,
      )
    );
  t.observers = remaining_observers;
};

let num_of_penguin_per_player = (state: GS.t): int =>
  6 - (List.length @@ GS.get_ordered_players(state));

/** ERRORS: if no player has [color] in [t].
    This abstracts out the mapping from color to player. */

let get_player_with_color = (t: t, color: Color.t): Player.t =>
  switch (List.Assoc.find(~equal=Color.equal, t.color_to_player, color)) {
  | None => failwith @@ "Color not found in referee: " ++ Color.show(color)
  | Some(player) => player
  };

/** EFFECT: Update [t.cheaters] or [t.failed] if [t.state] is populated.
    RETURN: the new game state, or [None] if all players are removed. */

let disqualify_current_player =
    (t: t, color: Color.t, why: [ | `Cheat | `Fail]): option(GS.t) =>
  Option.bind(
    read_state(t),
    ~f=state => {
      let player = get_player_with_color(t, color);
      let new_state = GS.remove_current_player(state);
      switch (why) {
      /* informing failed player could be dangerous */
      | `Fail => t.failed = [color, ...t.failed]
      | `Cheat =>
        t.cheaters = [color, ...t.cheaters];
        Core.ignore @@
        Timeout.call_with_timeout_ms(
          () => player#inform_disqualified(),
          t.conf.inform_disqualified_timeout_ms,
        );
      };
      player#dispose();
      inform_all_observers(
        t,
        [@implicit_arity] Disqualify(new_state, color),
      );
      new_state;
    },
  );

let handle_player_cheated = (t: t, color: Color.t): option(GS.t) =>
  disqualify_current_player(t, color, `Cheat);

let handle_player_failure = (t: t, color: Color.t): option(GS.t) =>
  disqualify_current_player(t, color, `Fail);

/** EFFECT: update [t.cheaters] or [t.failed] if current player cheats/fails.
    RETURN: final game state or [None] if all players are removed. */

let handle_current_player_penguin_placement = (t: t, gs: GS.t): option(GS.t) => {
  let board = GS.get_board_copy(gs);
  let color = GS.get_current_player(gs) |> PS.get_color;
  let player = get_player_with_color(t, color);
  let response =
    Option.join @@  /* timeout and communication failure are treated the same */
    Timeout.call_with_timeout_ms(
      () => player#place_penguin(gs),
      t.conf.placement_timeout_ms,
    );
  switch (response) {
  /* same treatment to timeout and communication failure */
  | None => handle_player_failure(t, color)
  | Some(pos) =>
    if (Board.within_board(board, pos)
        && (!) @@
        Tile.is_hole @@
        Board.get_tile_at(board, pos)) {
      let new_state =
        GS.rotate_to_next_player @@ GS.place_penguin(gs, color, pos);
      inform_all_observers(
        t,
        [@implicit_arity] PenguinPlacement(new_state, color, pos),
      );
      Option.some(new_state);
    } else {
      handle_player_cheated(t, color);
    }
  };
};

/** EFFECT: upadte [t.state], [t.cheaters] and [t.failed]. */

let handle_penguin_placement_phase = (t: t): unit => {
  let state = read_state(t); /* write is exclusively here during this phase */
  let penguins_per_player =
    Option.value_map(state, ~default=0, ~f=num_of_penguin_per_player);
  let all_players_have_enough_penguins = (state): bool =>
    List.map(~f=PS.get_penguins) @@
    GS.get_ordered_players(state)
    |> List.for_all(~f=pgs => penguins_per_player == List.length(pgs));

  let request_placement_or_skip_current_player = (state: GS.t): option(GS.t) => {
    let player_state = GS.get_current_player(state);
    if (penguins_per_player == List.length @@ PS.get_penguins(player_state)) {
      Option.some @@ GS.rotate_to_next_player(state);
    } else {
      handle_current_player_penguin_placement(t, state);
    };
  };

  /* EFFECT: update [t] after every placement */
  let rec loop = (state: GS.t): unit =>
    if ((!) @@ all_players_have_enough_penguins(state)) {
      let next_state_opt = request_placement_or_skip_current_player(state);
      write_state(t, next_state_opt);
      Option.iter(next_state_opt, ~f=loop);
    };

  Option.iter(~f=loop, state);
};

/** Skip on the player's behalf if it can't move.
    Return [None] if there was a communication failure or time out */

let get_player_action =
    (t: t, player: Player.t, tree: GT.t): option(Action.t) =>
  switch (GT.get_subtrees(tree)) {
  | [(Action.Skip, _)] => Option.some(Action.Skip)
  | _ =>
    Option.join @@  /* timeout and communication failure are treated the same */
    Timeout.call_with_timeout_ms(
      () => player#take_turn(tree),
      t.conf.turn_action_timeout_ms,
    )
  };

/** EFFECT: update [t.cheaters] or [t.failed] if current player cheats/fails.
    RETURN: final game tree or [None] if all players are removed. */

let handle_current_player_turn_action = (t: t, tree: GT.t): option(GT.t) => {
  let subtrees = GT.get_subtrees(tree);
  let state = GT.get_state(tree);
  let color = GS.get_current_player(state) |> PS.get_color;
  let player = get_player_with_color(t, color);
  switch (get_player_action(t, player, tree)) {
  | None => Option.map(~f=GT.create) @@ handle_player_failure(t, color)
  | Some(action) =>
    switch (List.Assoc.find(~equal=Action.equal, subtrees, action)) {
    | None => Option.map(~f=GT.create) @@ handle_player_cheated(t, color)
    | Some(next_sub_tree) =>
      let new_state = GT.get_state(next_sub_tree);
      inform_all_observers(
        t,
        [@implicit_arity] TurnAction(new_state, color, action),
      );
      Option.some(next_sub_tree);
    }
  };
};

/* EFFECT: upadte [t.state], [t.cheaters] and [t.failed]. */
let handle_turn_action_phase = (t: t): unit => {
  /* EFFECT: update [t] after every action */
  let rec loop = (tree: GT.t): unit =>
    switch (GT.get_subtrees(tree)) {
    | [] => () /* Game over */
    | _ =>
      let next_tree_opt = handle_current_player_turn_action(t, tree);
      write_state(t) @@ Option.map(~f=GT.get_state, next_tree_opt);
      Option.iter(next_tree_opt, ~f=loop);
    };

  Option.iter(~f=loop) @@ Option.map(~f=GT.create, read_state(t));
};

/** ASSUME: [t.color_to_player] has been properly instantiated.
    EFFECT: upadte [t.color_to_player], [t.state] and [t.failed]. */

let handle_color_assignment_phase = (t): unit => {
  /* assign color to current player and return resulting game state */
  let inform_player_with_color = (color, state): option(GS.t) => {
    let player = get_player_with_color(t, color);
    let result =
      Timeout.call_with_timeout_ms(
        () => player#assign_color(color),
        t.conf.assign_color_timeout_ms,
      );
    switch (result) {
    | Some(true) => Some(GS.rotate_to_next_player(state))
    | _ => handle_player_failure(t, color)
    };
  };

  /* EFFECT: update [t] after each color assignment */
  let rec go = (colors_to_inform, state): unit =>
    switch (colors_to_inform) {
    | [] => ()
    | [color, ...rest] =>
      let next_state_opt = inform_player_with_color(color, state);
      write_state(t, next_state_opt);
      Option.iter(next_state_opt, ~f=go(rest));
    };

  Option.iter(read_state(t), ~f=state =>
    go(List.map(~f=PS.get_color, GS.get_ordered_players(state)), state)
  );
};

/** Error if given invalid # of players */

let create_color_to_player_mapping_exn = (players): color_player_map => {
  let rec zip_to_shortest = (xs, ys) =>
    switch (xs, ys) {
    | ([], _)
    | (_, []) => []
    | ([x, ...xs], [y, ...ys]) => [(x, y), ...zip_to_shortest(xs, ys)]
    };

  let player_count = List.length(players);
  if (player_count < C.min_num_of_players
      || player_count > C.max_num_of_players) {
    failwith("Invalid number of players: " ++ string_of_int(player_count));
  } else {
    zip_to_shortest(C.init_colors, players);
  };
};

/** ASSUME: [t.color_to_player] has been instantiated properly.
    Fail if there aren't enough non-hole tiles to place penguins */

let create_and_validate_game_state_exn = (t, board_config): GS.t => {
  let board = Board.create(board_config);
  let colors = List.map(~f=Tuple.T2.get1, t.color_to_player);
  let state = GS.create(board, colors);
  let num_of_players = List.length @@ GS.get_ordered_players(state);
  let penguins_per_player = num_of_penguin_per_player(state);
  if (penguins_per_player
      * num_of_players > Board.num_of_non_hole_tiles(board)) {
    failwith(
      "Board doesn't have enough non-hole tiles for penguin placement",
    );
  } else {
    state;
  };
};

/** Compile final game result based on [t.state], [t.cheaters] and [t.failed] */

let collect_result = (t): Game_result.t => {
  let cheaters = List.map(~f=get_player_with_color(t), t.cheaters);
  let failed = List.map(~f=get_player_with_color(t), t.failed);
  switch (read_state(t)) {
  | None => {winners: [], rest: [], failed, cheaters}
  | Some(state) =>
    let players = GS.get_ordered_players(state);
    let max_score =
      List.map(~f=PS.get_score, players)
      |> List.max_elt(~compare=Int.compare)
      |> Option.value(~default=0);
    let winners =
      List.filter(~f=p => PS.get_score(p) == max_score, players)
      |> List.map(~f=Fn.compose(get_player_with_color(t), PS.get_color));

    let rest =
      List.filter(~f=p => PS.get_score(p) != max_score, players)
      |> List.map(~f=Fn.compose(get_player_with_color(t), PS.get_color));

    {winners, rest, failed, cheaters};
  };
};

let collect_and_report_result = (t): Game_result.t => {
  let result = collect_result(t);
  inform_all_observers(t, EndOfGame(result));
  result;
};

/** Update fields in [t] for starting a new game */

let init_referee_exn = (t, players, board_config) => {
  t.cheaters = [];
  t.failed = [];
  t.color_to_player = create_color_to_player_mapping_exn(players);
  write_state(t, Some(create_and_validate_game_state_exn(t, board_config)));
};

let default_timeout_config = {
  placement_timeout_ms: 3000,
  turn_action_timeout_ms: 3000,
  assign_color_timeout_ms: 3000,
  inform_disqualified_timeout_ms: 3000,
  inform_observer_timeout_ms: 3000,
};

let create = (~config=default_timeout_config, ()) => {
  state: None,
  cheaters: [],
  failed: [],
  observers: [],
  color_to_player: [],
  conf: config,
  state_lock: Mutex.create(),
};

let run_game = (t, players, board_config) => {
  init_referee_exn(t, players, board_config);
  Option.iter(read_state(t), ~f=state =>
    inform_all_observers(t, Register(state))
  );
  /* if all players are removed in any phase, [t.state] becomes [None] and
   * the control flow effectively short circuits all the way through */
  handle_color_assignment_phase(t);
  handle_penguin_placement_phase(t);
  handle_turn_action_phase(t);
  collect_and_report_result(t);
};
