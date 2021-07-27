open! Core;

/* Essentially a 2 list based circular queue */
type t('a) = {
  nexts: list('a), /* 1st element in [prevs] is the one after [current] */
  current: 'a,
  prevs: list('a) /* 1st element in [prevs] is the one before [current] */
};

/** Create a circular queue following the order of elements in [xs] */

let create_exn = (xs: list('a)) =>
  switch (xs) {
  | [] => failwith("An order must be non-empty")
  | [x, ...xs] => {nexts: xs, current: x, prevs: []}
  };

let create = (current, nexts) => {prevs: [], current, nexts};

let get_current = t => t.current;

let rotate = ({nexts, current, prevs}) =>
  switch (nexts) {
  | [] => create_exn @@ List.rev([current, ...prevs])
  | [n, ...ns] => {nexts: ns, current: n, prevs: [current, ...prevs]}
  };

let remove_current = t =>
  switch (t.nexts) {
  | [x, ...nexts] => Some({...t, current: x, nexts})
  | [] =>
    switch (List.rev(t.prevs)) {
    | [] => None
    | [x, ...prevs] => Some({prevs, current: x, nexts: []})
    }
  };

let to_list = ({nexts, current, prevs}) =>
  [current, ...nexts] @ List.rev(prevs); /* the one before [current] is the last */
