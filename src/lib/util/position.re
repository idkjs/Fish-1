open! Core;

[@deriving (equal, show)]
type t = {
  row: int,
  col: int,
};

let create_positions_within = (~height, ~width) => {
  open List.Let_syntax;
  let%bind row = List.init(height, ~f=Fun.id);
  let%bind col = List.init(width, ~f=Fun.id);
  return({row, col});
};

let compare = (p1: t, p2: t) => {
  let p1_cmp = Int.compare(p1.row, p2.row);
  if (p1_cmp == 0) {
    Int.compare(p1.col, p2.col);
  } else {
    p1_cmp;
  };
};
