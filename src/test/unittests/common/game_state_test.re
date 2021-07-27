open! Core;
module GS = Fish.Common.Game_state;
module B = Fish.Common.Board;
module Conf = Fish.Common.Board.Config;
module Color = Fish.Common.Player_state.Player_color;
module Pos = Fish.Util.Position;
module PS = Fish.Common.Player_state;
module PN = Fish.Common.Penguin;
module T = Fish.Common.Tile;

let tests =
  OUnit2.(>:::)(
    "game_state_tests",
    [
      OUnit2.(>::)("test_construction", _ => {
        let conf =
          Conf.create(~width=3, ~height=3)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes([]);
        let board = B.create(conf);
        let colors = [Color.Black, Color.Brown, Color.Red];
        let state = GS.create(board, colors);
        let players = List.map(~f=PS.create, colors);

        OUnit.assert_equal(board) @@ GS.get_board_copy(state);
        OUnit.assert_equal(players) @@ GS.get_ordered_players(state);
        OUnit.assert_equal(List.hd_exn(players)) @@
        GS.get_current_player(state);

        /* errors on invalid player colors */
        let expect = Failure("There must be at least 1 player in a game");
        OUnit2.assert_raises(expect, () => GS.create(board, []));
        let expect = Failure("Colors in a fish game must be unique");
        OUnit2.assert_raises(expect, () =>
          GS.create(board, [Color.Black, Color.Red, Color.Black])
        );
      }),
      OUnit2.(>::)("test_get_player_with_color", _ => {
        let conf =
          Conf.create(~width=3, ~height=3)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes([]);
        let board = B.create(conf);
        let colors = [Color.Black, Color.Brown, Color.Red];
        let state = GS.create(board, colors);

        List.iter(
          ~f=
            color =>
              OUnit.assert_equal(
                PS.create(color),
                GS.get_player_with_color(state, color),
              ),
          colors,
        );

        /* errors on unregistered color */
        let expect =
          Failure("No player has specified color in this game state");
        OUnit2.assert_raises(expect, () =>
          GS.get_player_with_color(state, PS.Player_color.White)
        );
      }),
      OUnit2.(>::)("test_remove_current_player", _ => {
        let conf =
          Conf.create(~width=3, ~height=3)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes([]);
        let board = B.create(conf);
        let colors = [Color.Black, Color.Brown, Color.Red];
        let state = GS.create(board, colors);
        let state = GS.place_penguin(state, Color.Red, {Pos.row: 1, col: 1});
        let state =
          GS.place_penguin(state, Color.Brown, {Pos.row: 0, col: 2});
        let players = GS.get_ordered_players(state);

        /* removing player has no effect on the board */
        let state = GS.remove_current_player(state);
        OUnit.assert_equal(
          Some(board),
          Option.map(~f=GS.get_board_copy, state),
        );
        /* removing middle player doesn't affect the overall player order */
        OUnit.assert_equal(
          Some(List.tl_exn(players)),
          Option.map(~f=GS.get_ordered_players, state),
        );

        /* error on removing last player */
        let state = Option.bind(~f=GS.remove_current_player, state);
        OUnit.assert_equal(
          Some(players |> List.tl_exn |> List.tl_exn),
          Option.map(~f=GS.get_ordered_players, state),
        );
        OUnit2.assert_equal(
          None,
          Option.bind(~f=GS.remove_current_player, state),
        );
      }),
      OUnit2.(>::)("test_rotate_players", _ => {
        let conf =
          Conf.create(~width=3, ~height=3)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes([]);
        let board = B.create(conf);
        let colors = [Color.Black, Color.Brown, Color.Red];
        let state = GS.create(board, colors);

        /* 2 rotations for all players */
        let state =
          List.fold_left(
            ~f=
              (gs, color) => {
                OUnit.assert_equal(PS.create(color)) @@
                GS.get_current_player(gs);
                GS.rotate_to_next_player(gs);
              },
            ~init=state,
            colors,
          );

        let state =
          List.fold_left(
            ~f=
              (gs, color) => {
                OUnit.assert_equal(PS.create(color)) @@
                GS.get_current_player(gs);
                GS.rotate_to_next_player(gs);
              },
            ~init=state,
            colors,
          );

        /* rotation shouldn't affect relative player order */
        let state = GS.rotate_to_next_player(state);
        let colors = [Color.Brown, Color.Red, Color.Black];
        OUnit.assert_equal(List.map(~f=PS.create, colors)) @@
        GS.get_ordered_players(state);

        /* Rotate with 1 single player */
        let state = GS.create(board, [PS.Player_color.White]);
        OUnit.assert_equal(PS.create(PS.Player_color.White)) @@
        GS.get_current_player(state);
        let state = GS.rotate_to_next_player(state);
        OUnit.assert_equal(PS.create(PS.Player_color.White)) @@
        GS.get_current_player(state);
      }),
      OUnit2.(>::)("test_place_penguin", _ => {
        let hole_pos = {Pos.row: 2, col: 2};
        let board =
          Conf.create(~width=3, ~height=3)
          |> Conf.set_default_num_of_fish(4)
          |> Conf.set_holes([hole_pos])
          |> B.create;

        let colors = [Color.Black, Color.Brown, Color.Red];
        let state0 = GS.create(board, colors);
        let pos11 = {Pos.row: 1, col: 1};
        let state1 = GS.place_penguin(state0, Color.Brown, pos11);
        let players = GS.get_ordered_players(state1);
        OUnit2.assert_equal(List.length(colors)) @@ List.length(players);
        let p = List.nth_exn(players, 1);
        OUnit2.assert_equal([PN.create(pos11)]) @@ PS.get_penguins(p);

        /* penguin placement shouldn't affect the board */
        OUnit2.assert_equal(
          GS.get_board_copy(state0),
          GS.get_board_copy(state1),
        );

        /* place another one */
        let pos02 = {Pos.row: 0, col: 2};
        let state1 = GS.place_penguin(state1, Color.Brown, pos02);
        let players = GS.get_ordered_players(state1);
        OUnit2.assert_equal(List.length(colors)) @@ List.length(players);
        let p = List.nth_exn(players, 1);
        OUnit2.assert_equal([PN.create(pos02), PN.create(pos11)]) @@
        PS.get_penguins(p);

        /* fails as expected when input is bad */
        let expect = Failure("Position is outside the board");
        OUnit2.assert_raises(expect, () =>
          GS.place_penguin(state0, Color.Brown, {Pos.row: 3, col: 3})
        );
        let expect = Failure("No player has given color");
        OUnit2.assert_raises(expect, () =>
          GS.place_penguin(state0, Color.White, pos11)
        );
        let expect = Failure("Cannot place penguin onto a hole");
        OUnit2.assert_raises(expect, () =>
          GS.place_penguin(state0, Color.White, hole_pos)
        );
        let expect =
          Failure(
            "Cannot place penguin onto a tile occupied by another penguin",
          );
        OUnit2.assert_raises(expect, () =>
          GS.place_penguin(state1, Color.White, pos11)
        );
      }),
      OUnit2.(>::)("test_move_penguin", _ => {
        /* Score and board should be updated.
         * Make sure game state "wires the 2 components up" */
        let hole_pos = {Pos.row: 0, col: 1};
        let board =
          Conf.create(~width=5, ~height=5)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes([hole_pos])
          |> B.create;

        let colors = [Color.Black, Color.Red];
        let pos11 = {Pos.row: 1, col: 1};
        let pos12 = {Pos.row: 1, col: 2};
        let pos13 = {Pos.row: 1, col: 3};
        let state0 = GS.create(board, colors);
        let state0 = GS.place_penguin(state0, Color.Red, pos11);
        let state1 = GS.move_penguin(state0, pos11, pos13);

        /* Board should be updated after move */
        let board = GS.get_board_copy(state1);
        let tile11 = B.get_tile_at(board, pos11);
        let tile13 = B.get_tile_at(board, pos13);
        OUnit2.assert_equal(true) @@ T.is_hole(tile11);
        OUnit2.assert_equal(false) @@ T.is_hole(tile13);
        OUnit2.assert_equal(3) @@ T.get_fish(tile13); /* fish not changed yet */

        /* Player should be updated after move */
        let players = GS.get_ordered_players(state1);
        OUnit2.assert_equal(List.length(colors)) @@ List.length(players);
        OUnit2.assert_equal(3) @@ PS.get_score @@ List.nth_exn(players, 1);
        OUnit2.assert_equal(0) @@ PS.get_score @@ List.nth_exn(players, 0);

        /* fails as expected when input is bad */
        let expect = Failure("Position is outside the board");
        OUnit2.assert_raises(expect, () =>
          GS.move_penguin(state1, {Pos.row: 5, col: 1}, pos12)
        );
        let expect = Failure("Position is outside the board");
        OUnit2.assert_raises(expect, () =>
          GS.move_penguin(state0, pos11, {Pos.row: 0, col: 6})
        );
        let expect = Failure("No penguin resides at source position");
        OUnit2.assert_raises(expect, () =>
          GS.move_penguin(state1, pos11, pos12)
        );
        let expect = Failure("Cannot move a penguin onto a hole");
        OUnit2.assert_raises(expect, () =>
          GS.move_penguin(state0, pos11, hole_pos)
        );
        let expect =
          Failure(
            "Cannot move penguin to a tile occupied by another penguin",
          );
        let state2 = GS.place_penguin(state1, Color.Black, pos12);
        OUnit2.assert_raises(expect, () =>
          GS.move_penguin(state2, pos13, pos12)
        );
      }),
      OUnit2.(>::)("test_get_board_minus_penguins", _ => {
        let board =
          Conf.create(~width=5, ~height=5)
          |> Conf.set_default_num_of_fish(3)
          |> B.create;

        let colors = [Color.Black, Color.Red];
        let pos11 = {Pos.row: 1, col: 1};
        let pos23 = {Pos.row: 2, col: 3};
        let state = GS.create(board, colors);
        let state = GS.place_penguin(state, Color.Red, pos11);
        let state = GS.place_penguin(state, Color.Red, pos23);
        let board = GS.get_board_minus_penguins(state);
        Pos.create_positions_within(~width=5, ~height=5)
        |> List.iter(~f=pos => {
             let tile = B.get_tile_at(board, pos);
             if (Pos.equal(pos, pos11) || Pos.equal(pos, pos23)) {
               OUnit2.assert_equal(true) @@ T.is_hole(tile);
             } else {
               OUnit2.assert_equal(false) @@ T.is_hole(tile);
             };
           });
      }),
    ],
  );

let _ = OUnit2.run_test_tt_main(tests);
