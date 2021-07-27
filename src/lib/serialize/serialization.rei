/** A [t] represents a serialized object */

type t;

/* NOTE some functions are for integration testing purposes
   to_* functions always return None if [t] is malformed */

/* Game state, source and destination positions of a penguin move */
let to_move_resp_query:
  t => result((Game_state.t, Position.t, Position.t), string);

/* board dimension (# of row, # of col), players with names and search depth for
 * minimax strategy, and default # of fish on tile */
let to_game_description:
  t => result((int, int, list(Player.t), int), string);

let from_pos: Position.t => t;
let to_pos: t => result(Position.t, string);

let from_color: Player_state.Player_color.t => t;
let to_color: t => result(Player_state.Player_color.t, string);

let from_action: Action.t => t;
let to_action: t => result(Action.t, string);

let from_board_posn: ((Board.t, Position.t)) => t;
let to_board_posn: t => result((Board.t, Position.t), string);

let from_game_state: Game_state.t => t;
let to_game_state: t => result(Game_state.t, string);

let from_list: (list('a), 'a => t) => t;
let to_list: (t, t => 'a) => result(list('a), string);

let from_string: string => t;
let to_string: t => option(string);

let from_bool: bool => t;
let to_bool: t => option(bool);

let from_int: int => t;
let to_int: t => option(int);

let deserialize: string => option(t);
let serialize: t => string;

let stream_from_channel: in_channel => Stream.t(t);
