module Move = {
  /* origin and destination for a penguin move */
  [@deriving (equal, show)]
  type t = {
    src: Position.t,
    dst: Position.t,
  };

  /** Compare 2 moves lexicalgraphically. */

  let compare = (m1: t, m2: t): int => {
    let src_cmp = Position.compare(m1.src, m2.src);
    if (src_cmp == 0) {
      Position.compare(m1.dst, m2.dst);
    } else {
      src_cmp;
    };
  };
};

/** A [t] represents one of the all possible action a player can submit to a
    referee, who will then validate and apply the action. */ /* Skip the current turn */

[@deriving (equal, show)]
type t =
  | Move(Move.t)
  | Skip;
