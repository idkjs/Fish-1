/** A [t] represents a tile on the fish game board , including:
    - whether it's a hole
    - # of fish on it
    NOTE that it's immutable */

[@deriving show]
type t;

/** A tile that represents a hole */

let hole: t;

/** Create a tile with given # of fish on it.
    Errors if input is not positive */

let create: int => t;

/** Return the # of fish on given tile, 0 if it's hole */

let get_fish: t => int;

/** Return whether the tile is a hole, i.e., removed */

let is_hole: t => bool;
