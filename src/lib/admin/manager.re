open! Core;

module Game_result = Referee.Game_result;
module BoardConf = Board.Config;

module Tournament_result = {
  type t = {
    final_winners: list(Player.t),
    all_losers: list(Player.t),
    all_cheaters: list(Player.t),
    all_failed_players: list(Player.t),
  };
};

type timeout_config = {
  inform_tournament_start_timeout_ms: int,
  inform_tournament_result_timeout_ms: int,
};

let default_timeout_config = {
  inform_tournament_start_timeout_ms: 3000,
  inform_tournament_result_timeout_ms: 3000,
};

/** A [t] represents the knowledge of a tournament manager */

type t = {
  active_players: list(Player.t),
  all_losers: list(Player.t),
  cheaters: list(Player.t),
  failed_players: list(Player.t),
  board_conf: BoardConf.t,
  timeout_conf: timeout_config,
  referee_timeout_conf: Referee.timeout_config,
};

let sort_players_by_age = (ps: list(Player.t)): list(Player.t) =>
  List.sort(ps, ~compare=(p1, p2) =>
    Int.compare(p1#get_age(), p2#get_age())
  );

/** NOTE referential equality is used here */

let is_same_set_of_players =
    (ps1: list(Player.t), ps2: list(Player.t)): bool => {
  module PlayerPair = {
    module T = {
      type t = Player.t;
      let compare = (p1: t, p2: t) =>
        Int.compare(Obj.magic(p1), Obj.magic(p2));
      let sexp_of_t = Core.sexp_of_opaque;
      let t_of_sexp = Core.opaque_of_sexp;
    };
    include T;
    include Comparable.Make(T);
  };

  Set.equal(
    Set.of_list((module PlayerPair), ps1),
    Set.of_list((module PlayerPair), ps2),
  );
};

/* If [f] returns [false], then the player is considered as "not responded".
   Dispose all failed players. */
let call_all_players_timeout =
    (players: list(Player.t), f: Player.t => bool, timeout_ms: int)
    : (list(Player.t), list(Player.t)) =>
  /* responded players and failed ones */
  List.fold_left(
    players, ~init=([], []), ~f=((responded_ones, failed_ones), p) =>
    switch (Timeout.call_with_timeout_ms(() => f(p), timeout_ms)) {
    | Some(true) => ([p, ...responded_ones], failed_ones)
    | _ =>
      p#dispose();
      (responded_ones, [p, ...failed_ones]);
    }
  );

let inform_players_tournament_start = (t: t): t => {
  let (responded, failed) =
    call_all_players_timeout(
      t.active_players,
      p => p#inform_tournament_start(),
      t.timeout_conf.inform_tournament_start_timeout_ms,
    );
  {
    ...t,
    active_players: responded,
    failed_players: failed @ t.failed_players,
  };
};

/** 1. assigning players to games with the maximal number of players per game.
    2. Once the number of remaining players drops below the minimal number,
       the manager backtracks by one game and tries games of size one
       less than the maximal number and so on until all players are assigned.
    NOTE the original order of players is preserved _within_ each group */

let allocate_players_to_games =
    (players: list(Player.t)): list(list(Player.t)) => {
  let rec loop = (players: list(Player.t), groups: list(list(Player.t))) =>
    if (List.length(players) < Referee.C.min_num_of_players) {
      /* TODO ASSUME min_num_of_players= 2. Algorithm is dicated by specs. */
      let prev_group = List.hd_exn(groups);
      let prev_groups = List.tl_exn(groups);
      let new_group = [List.hd_exn(prev_group), ...players];
      [new_group, List.tl_exn(prev_group), ...prev_groups];
    } else if (List.length(players) > Referee.C.max_num_of_players) {
      let (group, players) =
        List.split_n(players, Referee.C.max_num_of_players);
      loop(players, [group, ...groups]);
    } else {
      [players, ...groups];
    }; /* [List.length players] ∈ [min, max] */

  if (List.length(players) < Referee.C.min_num_of_players) {
    failwith("Insufficient # of players for game allocation");
  } else {
    loop(players, []);
  };
};

/* [t.active_players] will be winners from each round
   This is where parallelization may be implemented */
let run_games = (t: t, groups: list(list(Player.t))): t => {
  let t = {...t, active_players: []};
  List.fold_left(
    groups,
    ~init=t,
    ~f=(t, players) => {
      let referee = Referee.create(~config=t.referee_timeout_conf, ());
      let res = Referee.run_game(referee, players, t.board_conf);
      {
        ...t,
        active_players: res.winners @ t.active_players,
        cheaters: res.cheaters @ t.cheaters,
        failed_players: res.failed @ t.failed_players,
        all_losers: res.rest @ t.all_losers,
      };
    },
  );
};

/* [t.active_players] who responded become final winners.
 * Dispose all remaining players*/
let inform_and_compile_tournament_result = (t: t): Tournament_result.t => {
  let (final_winners, failed_winners) =
    call_all_players_timeout(
      t.active_players,
      p => p#inform_tournament_result(true),
      t.timeout_conf.inform_tournament_result_timeout_ms,
    );

  let (final_losers, failed_losers) =
    call_all_players_timeout(
      t.all_losers,
      p => p#inform_tournament_result(false),
      t.timeout_conf.inform_tournament_result_timeout_ms,
    );

  List.iter(~f=p => p#dispose(), final_winners @ final_losers);
  {
    final_winners,
    all_losers: final_losers,
    all_cheaters: t.cheaters,
    all_failed_players: failed_losers @ failed_winners @ t.failed_players,
  };
};

let run_tournament =
    (
      ~timeout_conf=default_timeout_config,
      ~referee_timeout_conf=Referee.default_timeout_config,
      players,
      board_conf,
    ) => {
  /* ASSUME: [(List.length t.active_players) > Referee.C.min_num_of_players] */
  let rec loop = (t: t, prev_winners: list(Player.t)): Tournament_result.t => {
    let t = {...t, active_players: sort_players_by_age(t.active_players)};
    let groups = allocate_players_to_games(t.active_players);
    let t = run_games(t, groups);
    if (List.length(t.active_players) < Referee.C.min_num_of_players
        || List.length(groups) == 1  /* only 1 game was run */
        || is_same_set_of_players(t.active_players, prev_winners)) {
      inform_and_compile_tournament_result(t);
    } else {
      loop(t, t.active_players);
    };
  };

  let t = {
    active_players: players,
    all_losers: [],
    cheaters: [],
    failed_players: [],
    board_conf,
    timeout_conf,
    referee_timeout_conf,
  };
  let t = inform_players_tournament_start(t);
  if (List.length(t.active_players) < Referee.C.min_num_of_players) {
    inform_and_compile_tournament_result(t);
  } else {
    loop(t, []);
  };
};
