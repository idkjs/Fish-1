module Game_result: {
  type t = {
    winners: list(Player.t),
    cheaters: list(Player.t),
    failed: list(Player.t),
    rest: list(Player.t),
  };
};

module Game_observer: {
  type event =
    /* invoked when the game start, or [t] is registered during a game.
       Each event contains the most up-to-date game state */
    | Register(Game_state.t)
    | PenguinPlacement(Game_state.t, Player_state.Player_color.t, Position.t)
    | TurnAction(Game_state.t, Player_state.Player_color.t, Action.t)
    | Disqualify(option(Game_state.t), Player_state.Player_color.t)
    | EndOfGame(Game_result.t);
  type t = event => unit;
};

/** A [t] represents a referee which manages an entire fish game from start to
    end. It can:
    - Set up and run a game given a board config and ordered list of [Player.t]s
    - Add [Game_observer.t] before or during a game, and periodically report
      game events to the observers.
    - Report final result of a game after it's finished */

type t;

/* Constants */
module C: {
  let min_num_of_players: int;
  let max_num_of_players: int;
};

/* Enable clients to override default time out configurations */
type timeout_config = {
  assign_color_timeout_ms: int,
  placement_timeout_ms: int,
  turn_action_timeout_ms: int,
  inform_disqualified_timeout_ms: int,
  inform_observer_timeout_ms: int,
};
let default_timeout_config: timeout_config;

/** Create a referee, waiting to run a game */

let create: (~config: timeout_config=?, unit) => t;

/** EFFECT: update [t] to keep track of given observer and inform it of ongoing
    game events (if a game is running) */

let add_game_observer: (t, Game_observer.t) => unit;

/** Set up and run a game with the given list of players and board config.
    The turn order is determined by the player order in the original list.
    The initial board is constructed based on given config.
    Each player gets a fixed # of penguins to place, then the game continues
    until either no player can make a move, or everyone is kicked out.
    NOTE cheaters and failed players will be disposed immediately.
    Error if
    - the # of players is outside the range specified in constants above.
    - we can't create a valid board based on given config.
    - the board doesn't have enough non-hole tiles for penguin placement. */

let run_game: (t, list(Player.t), Common.Board.Config.t) => Game_result.t;
