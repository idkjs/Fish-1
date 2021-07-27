module Player_color = Player_state.Player_color;

/** A [t] represents a snapshot of the game state at a certain time.
    It includes:
      - the board state
      - state of all participating players
      - the order in which players take turn, and the current player
    It excludes:
      - how to communicate with the actual players
      - what constitutes a "legal" move according to some set of tules
    It ensures:
      - all players have distinct colors, and there is at least 1 player.
      - all penguins reside on non-hole, within-board, and distinct tiles.
    NOTE that it's immutable */

type t;

/** Create a game state with given board and participating players
    Errors if there are duplicates in the colors, or if the list is empty. */

let create: (Board.t, list(Player_color.t)) => t;

let get_board_copy: t => Board.t;

/** Return a list of players where the first player is the current player, and
    the rest conforms with their turn order in the game state. */

let get_ordered_players: t => list(Player_state.t);

/** Return the player that has the given color in [t]
    Errors if no player has the specified color */

let get_player_with_color: (t, Player_color.t) => Player_state.t;

/** Return a board after removing all tiles that have a penguin on it */

let get_board_minus_penguins: t => Board.t;

/** Return the current player in [t] */

let get_current_player: t => Player_state.t;

/** Rotate current player to the next player */

let rotate_to_next_player: t => t;

/** Remove the current player in this state, and leave the board unaffected.
    Return [None] if the state has no player left after removal. */

let remove_current_player: t => option(t);

/** Place a new penguin with given color at given position on the board.
    Errors if
    - no the participating player has given color
    - position is out of bound or is a hole
    - a penguin already exists at the position */

let place_penguin: (t, Player_color.t, Position.t) => t;

/** Move the penguin at 1st position to the 2nd position, and update player
    score accordingly.
    Errors if
    - any position is out of bound
    - no penguin exists at source position
    - target position is a hole
    - a penguin already exists at the target position */

let move_penguin: (t, Position.t, Position.t) => t;

/** Discouraged unless you have good reason and know what you are doing
    Return error with a reason if any internal invariant is broken. */

let from_board_players: (Board.t, list(Player_state.t)) => result(t, string);
