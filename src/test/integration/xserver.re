open! Core;
module S = Fish.Serialize.Serialization;
module BoardConf = Fish.Common.Board.Config;
module Signup = Fish.Remote.Signup_manager;
module Manager = Fish.Admin.Manager;

let signup_config = {
  Signup.min_num_of_players: 5,
  max_num_of_players: 10,
  num_of_waiting_periods: 2,
  waiting_period_ms: 30_000,
  name_reply_timeout_ms: 10_000,
  max_name_length: 12,
  max_pending_reqs: 80,
};

let board_config =
  BoardConf.create(~width=5, ~height=5)
  |> BoardConf.set_holes([])
  |> BoardConf.set_min_num_of_one_fish_tile(0)
  |> BoardConf.set_default_num_of_fish(2);

let parse_port = (args: array(string)): option(int) =>
  if (Array.length(args) != 2) {
    None;
  } else {
    int_of_string_opt(args[1]);
  };

/* Start up a server for fish game at specified port */
let () =
  switch (parse_port @@ Sys.get_argv()) {
  | None => Printf.printf("Please specify a single integer for port\n")
  | Some(port) =>
    let players = Signup.sign_up(signup_config, ~port);
    if (List.length(players) >= signup_config.min_num_of_players) {
      let result = Manager.run_tournament(players, board_config);
      let winner_count = List.length(result.final_winners);
      let cheater_count = List.length(result.all_cheaters);
      let failde_player_count = List.length(result.all_failed_players);
      S.from_list(
        [winner_count, cheater_count + failde_player_count],
        S.from_int,
      )
      |> S.serialize
      |> Printf.printf("%s\n");
    };
  };
