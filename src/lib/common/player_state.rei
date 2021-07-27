/** A [t] represents the state of a player in a fish game, including
    - color
    - score (non-negative)
    - all of its penguins
    NOTE
    - it's immutable
    - [t] itself doesn't hold any invariants on its penguins. This is left to
      more high level data representations. */

type t;

module Player_color: {
  /** A [t] represents the color of player and their penguins in a Fish game */

  [@deriving (show, compare, equal)]
  type t =
    | Red
    | Brown
    | Black
    | White;
};

let create: Player_color.t => t;
let get_color: t => Player_color.t;

/** Errors if score is negative */

let set_score: (t, int) => t;
let get_score: t => int;

/** Move the penguin at 1st position to the 2nd position.
    Return None if player has no penguin at 1st position */

let move_penguin: (t, Position.t, Position.t) => option(t);
let add_penguin: (t, Penguin.t) => t;

/** Return all penguins owned by [t], reverse to the order they were added */

let get_penguins: t => list(Penguin.t);
