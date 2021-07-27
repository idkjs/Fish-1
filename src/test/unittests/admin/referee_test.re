open! Core;
module Conf = Fish.Common.Board.Config;
module Pos = Fish.Util.Position;
module Referee = Fish.Admin.Referee;
module Player = Fish.Player;

let conf =
  Conf.create(~width=5, ~height=5)
  |> Conf.set_default_num_of_fish(3)
  |> Conf.set_holes([])
  |> Conf.set_min_num_of_one_fish_tile(0);

/* Mock and AI players are fast enough here anyway */
let fast_timeout_config = {
  ...Referee.default_timeout_config,
  turn_action_timeout_ms: 100,
  placement_timeout_ms: 100,
  assign_color_timeout_ms: 100,
  inform_disqualified_timeout_ms: 100,
};

let tests =
  OUnit2.(>:::)(
    "referee_test",
    [
      OUnit2.(>::)("test_run_game_inputs_validation", _ =>
        /* Use different board config and # of players to make sure [run_game]
         * properly errors on invalid inputs */
        let holes = [
          {Pos.row: 0, col: 1},
          {Pos.row: 0, col: 2},
          {Pos.row: 1, col: 0},
        ];
        let conf =
          Conf.create(~width=3, ~height=3)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes(holes);
        let conf_no_hole = Conf.set_holes([], conf);
        let referee = Referee.create();

        /* too few players */
        OUnit.assert_raises(Failure("Invalid number of players: 0"), () =>
          Referee.run_game(referee, [], conf)
        );
        OUnit.assert_raises(Failure("Invalid number of players: 1"), () =>
          Referee.run_game(referee, [Util.get_default_ai_player(0)], conf)
        );

        /* these should pass without error */
        Core.ignore @@
        Referee.run_game(
          referee,
          List.init(2, ~f=Util.get_default_ai_player),
          conf_no_hole,
        );
        Core.ignore @@
        Referee.run_game(
          referee,
          List.init(3, ~f=Util.get_default_ai_player),
          conf_no_hole,
        );
        Core.ignore @@
        Referee.run_game(
          referee,
          List.init(4, ~f=Util.get_default_ai_player),
          conf_no_hole,
        );

        /* too many players */
        OUnit.assert_raises(Failure("Invalid number of players: 5"), () =>
          Referee.run_game(
            referee,
            List.init(5, ~f=Util.get_default_ai_player),
            conf_no_hole,
          )
        );
        OUnit.assert_raises(Failure("Invalid number of players: 6"), () =>
          Referee.run_game(
            referee,
            List.init(6, ~f=Util.get_default_ai_player),
            conf_no_hole,
          )
        );

        /* not enough open tiles for penguin placement */
        let msg = "Board doesn't have enough non-hole tiles for penguin placement";
        OUnit.assert_raises(Failure(msg), () =>
          Referee.run_game(
            referee,
            List.init(4, ~f=Util.get_default_ai_player),
            conf,
          )
        );
      ),
      OUnit2.(>::)("test_run_game_all_fail_at_color_assignment", _ =>
        let referee = Referee.create(~config=fast_timeout_config, ());
        let players =
          List.init(
            4,
            ~f=
              Fn.compose(
                Mock_players.get_player_hang_at_color_assignment,
                string_of_int,
              ),
          );
        let result = Referee.run_game(referee, players, conf);
        OUnit.assert_equal([], result.winners);
        OUnit.assert_equal([], result.cheaters);
        OUnit.assert_equal([], result.rest);
        Util.check_same_set_of_players_by_names(players, result.failed);
      ),
      OUnit2.(>::)(
        "test_run_game_all_fail_at_color_assignment_and_disqualification", _ =>
        let referee = Referee.create(~config=fast_timeout_config, ());
        let players =
          List.init(
            4,
            ~f=
              Fn.compose(
                Mock_players.get_player_hang_at_color_assignment_and_disqualification,
                string_of_int,
              ),
          );
        let result = Referee.run_game(referee, players, conf);
        OUnit.assert_equal([], result.winners);
        OUnit.assert_equal([], result.cheaters);
        OUnit.assert_equal([], result.rest);
        Util.check_same_set_of_players_by_names(players, result.failed);
      ),
      OUnit2.(>::)("test_run_game_all_fail/cheat_at_penguin_placement", _ =>
        let referee = Referee.create(~config=fast_timeout_config, ());
        let cheaters = [
          Mock_players.get_player_cheat_at_placement("cheat-1"),
          Mock_players.get_player_cheat_at_placement("cheat-0"),
        ];
        let failed = [
          Mock_players.get_player_fail_at_placement("fail-0"),
          Mock_players.get_player_hang_at_placement("hang-1"),
        ];
        let players = failed @ cheaters;
        let result = Referee.run_game(referee, players, conf);
        OUnit.assert_equal([], result.winners);
        OUnit.assert_equal([], result.rest);
        Util.check_same_set_of_players_by_names(failed, result.failed);
        Util.check_same_set_of_players_by_names(cheaters, result.cheaters);
      ),
      OUnit2.(>::)("test_run_game_all_fail/cheat_at_turn_action", _ =>
        let referee = Referee.create(~config=fast_timeout_config, ());
        let cheaters = [
          Mock_players.get_player_cheat_at_turn_action("cheat-1"),
          Mock_players.get_player_cheat_at_turn_action("cheat-0"),
        ];
        let failed = [
          Mock_players.get_player_fail_at_turn_action("fail-0"),
          Mock_players.get_player_hang_at_turn_action("hang-1"),
        ];
        let players = failed @ cheaters;
        let result = Referee.run_game(referee, players, conf);
        OUnit.assert_equal([], result.winners);
        OUnit.assert_equal([], result.rest);
        Util.check_same_set_of_players_by_names(failed, result.failed);
        Util.check_same_set_of_players_by_names(cheaters, result.cheaters);
      ),
      OUnit2.(>::)("test_run_game_mix_good_and_bad_players_multiple_runs", _ =>
        let referee = Referee.create(~config=fast_timeout_config, ());
        let winners = [Util.get_default_ai_player(42)];
        let losers = [Util.get_default_ai_player(23)];
        let cheaters = [
          Mock_players.get_player_cheat_at_turn_action("cheat-0"),
        ];
        let failed = [Mock_players.get_player_fail_at_placement("fail-0")];
        let players = losers @ winners @ failed @ cheaters;
        let result = Referee.run_game(referee, players, conf);
        Util.check_same_set_of_players_by_names(winners, result.winners);
        Util.check_same_set_of_players_by_names(losers, result.rest);
        Util.check_same_set_of_players_by_names(failed, result.failed);
        Util.check_same_set_of_players_by_names(cheaters, result.cheaters);

        /* Use the same referee for multiple times */
        let winners = [Util.get_default_ai_player(42)];
        let losers = [Util.get_default_ai_player(23)];
        let cheaters = [
          Mock_players.get_player_cheat_at_placement("cheat-0"),
        ];
        let failed = [Mock_players.get_player_fail_at_turn_action("fail-0")];
        let players = losers @ winners @ failed @ cheaters;
        let result = Referee.run_game(referee, players, conf);
        Util.check_same_set_of_players_by_names(winners, result.winners);
        Util.check_same_set_of_players_by_names(losers, result.rest);
        Util.check_same_set_of_players_by_names(failed, result.failed);
        Util.check_same_set_of_players_by_names(cheaters, result.cheaters);

        let winners = [Util.get_default_ai_player(42)];
        let cheaters = [
          Mock_players.get_player_cheat_at_placement("cheat-0"),
        ];
        let failed = [
          Mock_players.get_player_hang_at_color_assignment("fail-0"),
          Mock_players.get_player_hang_at_color_assignment_and_disqualification(
            "fail-2",
          ),
        ];
        let players = winners @ failed @ cheaters;
        let result = Referee.run_game(referee, players, conf);
        Util.check_same_set_of_players_by_names(winners, result.winners);
        OUnit2.assert_equal([], result.rest);
        Util.check_same_set_of_players_by_names(failed, result.failed);
        Util.check_same_set_of_players_by_names(cheaters, result.cheaters);
      ),
    ],
  );
let _ = OUnit2.run_test_tt_main(tests);
