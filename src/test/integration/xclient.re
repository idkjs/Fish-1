open! Core;

module Remote_player = Fish.Remote.Remote_player;
module Player = Fish.Player;

type config = {
  num_clients: int,
  search_depth: int,
  port: int,
  server_ip: string,
};

let parse_config = (args: array(string)): option(config) => {
  let size = Array.length(args);
  if (size != 3 && size != 4) {
    None;
  } else {
    open Option.Let_syntax;
    let%bind num_clients = int_of_string_opt(args[1]);
    let%bind port = int_of_string_opt(args[2]);
    let server_ip =
      if (size == 3) {
        "127.0.0.1";
      } else {
        args[3];
      };
    Option.return({num_clients, port, server_ip, search_depth: 2});
  };
};

let create_ith_player = (conf: config, i: int): Player.t =>
  Player.create_AI_player(
    ~name=Printf.sprintf("OCaml-AI-%d", i),
    ~age=i,
    Player.Strategy.Penguin_placer.create_scanning_strategy,
    Player.Strategy.Turn_actor.create_minimax_strategy(conf.search_depth),
  );

let create_ith_thread = (conf: config, i: int): Thread.t => {
  let player = create_ith_player(conf, i);
  let (ipaddr, port) = (conf.server_ip, conf.port);
  Thread.create(
    () => Remote_player.interact_with_proxy(player, ~ipaddr, ~port),
    ~on_uncaught_exn=`Print_to_stderr,
    (),
  );
};

/* Start up a server for fish game at specified port */
let () =
  switch (parse_config @@ Sys.get_argv()) {
  | None =>
    Printf.printf("Input: <number-of-clients> <port-number> <ip-address>")
  | Some(conf) =>
    List.init(conf.num_clients, ~f=create_ith_thread(conf))
    |> List.iter(~f=Thread.join)
  };
