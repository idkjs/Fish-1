open! Core;

module Timeout = Util.Timeout;
module S = Serialize.Serialization;

type config = {
  min_num_of_players: int,
  max_num_of_players: int,
  num_of_waiting_periods: int,
  waiting_period_ms: int,
  name_reply_timeout_ms: int,
  max_name_length: int,
  max_pending_reqs: int,
};

/* A mutable timer to keep track of the amount of time left */
module Timer = {
  type t = {
    mutable start_ms: float,
    mutable time_left_ms: float,
  };

  let create = (time_left_ms: int) => {
    start_ms: 1000. *. Unix.gettimeofday(),
    time_left_ms: float_of_int(time_left_ms),
  };

  /* Return time_left_ms relative to the creation of this timer.
   * EFFECT: update [t.start_ms] and [t.time_left_ms] */
  let get_time_left_ms = (t: t): int => {
    let new_start_ms = 1000. *. Unix.gettimeofday();
    let time_passed_ms = new_start_ms -. t.start_ms;
    t.time_left_ms = t.time_left_ms -. time_passed_ms;
    t.start_ms = new_start_ms;
    int_of_float(t.time_left_ms);
  };
};

/** Return a socket that is ready to accpet connections */

let create_server = (conf: config, port: int): Unix.File_descr.t => {
  let sockaddr =
    [@implicit_arity] Unix.ADDR_INET(Unix.Inet_addr.localhost, port);
  let domain = Unix.domain_of_sockaddr(sockaddr);
  let sock = Unix.socket(~domain, ~kind=SOCK_STREAM, ~protocol=0, ());
  Unix.bind(sock, ~addr=sockaddr);
  Unix.listen(sock, ~backlog=conf.max_pending_reqs);
  sock;
};

let is_valid_name = (conf: config, name: string): bool =>
  (!) @@
  String.is_empty(name)
  && String.length(name) <= conf.max_name_length
  && String.for_all(name, ~f=Char.is_alpha);

let handle_waiting_period =
    (
      server: Unix.File_descr.t,
      conf: config,
      connected_players: list(Player.t),
    )
    : list(Player.t) => {
  let timer = Timer.create(conf.waiting_period_ms);
  /* Return [None] on invalid name, comm failure or [timer] expiration */
  let read_and_validate_name = (ic: In_channel.t): option(string) => {
    open Option.Let_syntax;
    let%bind opt_name =
      Timeout.call_with_timeout_ms(
        () =>
          try(S.stream_from_channel(ic) |> Stream.next |> S.to_string) {
          | _ => None /* catch io exceptions */
          },
        Int.min(Timer.get_time_left_ms(timer), conf.name_reply_timeout_ms),
      );

    let%bind name = opt_name;
    Option.some_if(is_valid_name(conf, name), name);
  };

  /* Keep accepting new player until maximum # is reached or [timer] expires */
  let rec loop = (players: list(Player.t)): list(Player.t) =>
    if (List.length(players) >= conf.max_num_of_players
        || Timer.get_time_left_ms(timer) < 0) {
      players;
    } else {
      switch (
        Timeout.call_with_timeout_ms(
          () => Unix.accept(server),
          Timer.get_time_left_ms(timer),
        )
      ) {
      | None => players /* this waiting period is over */
      | Some((client_sock, _)) =>
        loop @@ try_to_add_client(players, client_sock)
      };
    }
  /* Add [client_sock] as a proxy player to [players] if it sends in a valid
   * name before [timer] expires */
  and try_to_add_client =
      (players: list(Player.t), client_sock: Unix.File_descr.t)
      : list(Player.t) =>
    /* catch io exceptions */
    try({
      let ic = Unix.in_channel_of_descr(client_sock);
      let oc = Unix.out_channel_of_descr(client_sock);
      switch (read_and_validate_name(ic)) {
      | None =>
        Unix.close(client_sock);
        players; /* properly release resources */
      | Some(name) =>
        let age = List.length(players);
        [Remote_player.create_proxy_player(ic, oc, ~name, ~age), ...players];
      };
    }) {
    | _ =>
      Unix.close(client_sock);
      players;
    }; /* properly release resources */

  /* Chose not to use Timeout module for end of waiting period because we need
   * to properly release socket resources */
  loop(connected_players);
};

let sign_up = (conf, ~port) => {
  let rec loop =
          (
            server: Unix.File_descr.t,
            num_of_wp_left: int,
            players: list(Player.t),
          )
          : list(Player.t) =>
    if (num_of_wp_left == 0) {
      players;
    } else {
      let players = handle_waiting_period(server, conf, players);
      if (List.length(players) > conf.min_num_of_players) {
        players;
      } else {
        loop(server, num_of_wp_left - 1, players);
      };
    };

  let server = create_server(conf, port);
  let signed_up_players =
    try(loop(server, conf.num_of_waiting_periods, [])) {
    | error =>
      Unix.close(server);
      raise(error);
    };

  Unix.close(server);
  signed_up_players;
};
