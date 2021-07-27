/** A [t] represents a position on the fish game board. Check @see 'game/board'
    for how a position is interpreted. */

[@deriving (equal, show)]
type t = {
  row: int,
  col: int,
};

/** Creates a list of distinct positions (row, col) for
    0 <= row < [height] and 0 <= column < [width] */

let create_positions_within: (~height: int, ~width: int) => list(t);

/** Compare 2 positions lexicalgraphically.
    ex: (0, 0), (1, 2) -> -1 */

let compare: (t, t) => int;
