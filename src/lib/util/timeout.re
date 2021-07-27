open! Core;
module Sys = Stdlib.Sys;

/* TODO This probably can't be used by multiple threads concurrently */
exception Timeout;
let call_with_timeout_ms = (thunk: unit => 'a, ms: int): option('a) => {
  Core.ignore @@  /* time out once after [ms] ms */
  Unix.setitimer(
    Unix.ITIMER_REAL,
    {Unix.it_interval: 0.0, it_value: float_of_int(ms) /. 1000.},
  );
  Sys.set_signal(Sys.sigalrm, Sys.Signal_handle(_ => raise(Timeout)));
  try({
    let res = Some(thunk());
    Core.ignore @@  /* cancel timeout since the computation is finished */
    Unix.setitimer(Unix.ITIMER_REAL, {Unix.it_interval: 0.0, it_value: 0.0});
    res;
  }) {
  | Timeout => None
  };
};
