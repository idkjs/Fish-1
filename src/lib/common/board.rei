/** A [t] represents the hexagon board in a fish game.
    The graphical interpretatio for positions (row, col) is as follows
    ------        ------
   /(0, 0)\------/(0, 1)\
   \------/(1, 0)\------/
   /(2, 0)\------/(2, 1)\
   \------/(3, 0)\------/
          \------/
    NOTE mutability is implementation dependent, and should not be assumed.
    Effectful function always return the updated object (mutable or not)
*/

type t;

module Direction: {
  type t =
    | North
    | South
    | Northeast
    | Southeast
    | Northwest
    | Southwest;

  /** Return the resulting position after taking 1 step
      from given position in given direction */

  let step_from: (Position.t, t) => Position.t;
};

module Config: {
  /** A [t] represents configuration for a board. With the following paramters:
      - Width and height.
      - position of holes (removed tiles) on board. (ignored if out of bound)
      - minimum number of tiles with exactly 1 fish. (might not be enforced)
      - default number of fish on a tile. */

  type t;

  /** Create a configuration with given [width] and [height] */

  let create: (~height: int, ~width: int) => t;
  /* NOTE input config is passed at the end to enable chaining */
  let set_width: (int, t) => t;
  let get_width: t => int;
  let set_height: (int, t) => t;
  let get_height: t => int;
  let set_holes: (list(Position.t), t) => t;
  let get_holes: t => list(Position.t);
  let set_min_num_of_one_fish_tile: (int, t) => t;
  let get_min_num_of_one_fish_tile: t => int;
  let set_default_num_of_fish: (int, t) => t;
  let get_default_num_of_fish: t => int;
};

/** Create a board with given configuration
    Error if any dimenson is non-positive. */

let create: Config.t => t;

/** Return the number of columns. */

let get_width: t => int;

/** Return the number of rows. */

let get_height: t => int;

/** Return the position of the top-left tile. */

let get_top_left_pos: t => Position.t;

/** Whether the given position is within the board, i.e., a valid position  */

let within_board: (t, Position.t) => bool;

/** Retrive the tile at given position.
    Errors if the position is out of bound */

let get_tile_at: (t, Position.t) => Tile.t;

/** Remove the tile at given position.
    Errors if the position is out of bound */

let remove_tile_at: (t, Position.t) => t;

/** Return the # of tiles that aren't holes on given board */

let num_of_non_hole_tiles: t => int;

/** Return all positions reachable from given position on the board via straight
    lines following each direction in [Direction.t]. Return a list that
    associates each direction with the reachable positions ordered by their
    distance from origin */

let get_reachable_from:
  (t, Position.t) => list((Direction.t, list(Position.t)));

/** NOTE if [t] is immutable, this is just identity function */

let get_copy: t => t;

/** Width is determined by the longest row. Short rows will be filled with hole
    tiles. Errors if width or height is 0. */

let from_tiles: list(list(Tile.t)) => result(t, string);
