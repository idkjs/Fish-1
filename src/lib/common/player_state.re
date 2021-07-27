module Player_color = {
  /** A [t] represents the color of player and their penguins in a Fish game */

  [@deriving (show, compare, equal)]
  type t =
    | Red
    | Brown
    | Black
    | White;
};

type t = {
  color: Player_color.t,
  score: int,
  penguins: list(Penguin.t),
};

let create = color => {color, score: 0, penguins: []};

let get_color = t => t.color;

let set_score = (t, score) =>
  if (score >= 0) {
    {...t, score};
  } else {
    failwith("score must be non-negative");
  };

let get_score = t => t.score;

let move_penguin = (t, src, dst) => {
  let rec update_penguin = pgs =>
    switch (pgs) {
    | [] => None
    | [p, ...pgs] =>
      if (src === Penguin.get_position(p)) {
        Some([Penguin.set_position(p, dst), ...pgs]);
      } else {
        Option.map(List.cons(p), update_penguin(pgs));
      }
    };

  Option.map(penguins => {...t, penguins}) @@ update_penguin(t.penguins);
};

let add_penguin = (t, p) => {...t, penguins: [p, ...t.penguins]};

let get_penguins = t => t.penguins;
