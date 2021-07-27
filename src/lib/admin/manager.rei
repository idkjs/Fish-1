module Tournament_result: {
  type t = {
    /* each list of players is in no particular order */
    final_winners: list(Player.t),
    all_losers: list(Player.t),
    all_cheaters: list(Player.t),
    all_failed_players: list(Player.t),
  };
};

/* Enable clients to override default time out configurations */
type timeout_config = {
  inform_tournament_start_timeout_ms: int,
  inform_tournament_result_timeout_ms: int,
};
let default_timeout_config: timeout_config;

/** The top finisher(s) of every game of round n move on to round n+1.
    Termination Conditions (disjunctions) :
    1. two tournament rounds of games in a row produce the exact same winners
    2. when there are too few players for a single game,
    3. when the number of participants has become small enough to run a single
       final game (and yes this game is run).
    The order of players in each game is based on their age (youngest first).
    NOTE all players will be disposed when this function call returns. */

let run_tournament:
  (
    ~timeout_conf: timeout_config=?,
    ~referee_timeout_conf: Referee.timeout_config=?,
    list(Player.t),
    Common.Board.Config.t
  ) =>
  Tournament_result.t;
