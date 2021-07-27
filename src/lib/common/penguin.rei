/** A [t] represents a penguin in a fish game, including:
    - where it is
    NOTE that it's immutable */

type t;

/** Creating a penguin at given position */

let create: Position.t => t;

let set_position: (t, Position.t) => t;
let get_position: t => Position.t;
