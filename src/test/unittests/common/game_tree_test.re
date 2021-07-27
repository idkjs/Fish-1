module GT = Fish.Common.Game_tree;
module GS = Fish.Common.Game_state;
module B = Fish.Common.Board;
module Conf = Fish.Common.Board.Config;
module Color = Fish.Common.Player_state.Player_color;
module Pos = Fish.Util.Position;
module Action = Fish.Common.Action;

let tests =
  OUnit2.(>:::)(
    "game_tree_test",
    [
      OUnit2.(>::)("test_construction", _ =>
        let conf =
          Conf.create(~width=3, ~height=3)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes([]);
        let board = B.create(conf);
        let colors = [Color.Black, Color.Brown, Color.Red];
        let state = GS.create(board, colors);
        let tree = GT.create(state);
        OUnit.assert_equal(state) @@ GT.get_state(tree);
      ),
      OUnit2.(>::)("test_get_subtrees_end", _ =>
        /* <red>   <0,1>   <--->
         *     <--->   <1,1>   <bro> */
        let red_pos = {Pos.row: 0, col: 0};
        let brown_pos = {Pos.row: 1, col: 2};
        let hole1 = {Pos.row: 1, col: 0};
        let hole2 = {Pos.row: 0, col: 2};
        let conf =
          Conf.create(~width=3, ~height=2)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes([hole1, hole2]);
        let board = B.create(conf);
        let colors = [Color.Red, Color.Brown, Color.Black];
        let state = GS.create(board, colors);
        let state = GS.place_penguin(state, Color.Red, red_pos);
        let state = GS.place_penguin(state, Color.Brown, brown_pos);
        let tree = GT.create(state);
        OUnit.assert_equal([]) @@ GT.get_subtrees(tree);
      ),
      OUnit2.(>::)("test_get_subtrees_skip", _ =>
        /* <red>   <0,1>   <0,2>
         *     <--->   <bro>   <1,2> */
        let red_pos = {Pos.row: 0, col: 0};
        let brown_pos = {Pos.row: 1, col: 1};
        let hole_pos = {Pos.row: 1, col: 0};
        let conf =
          Conf.create(~width=3, ~height=2)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes([hole_pos]);
        let board = B.create(conf);
        let colors = [Color.Red, Color.Brown, Color.Black];
        let state = GS.create(board, colors);
        let state = GS.place_penguin(state, Color.Red, red_pos);
        let state = GS.place_penguin(state, Color.Brown, brown_pos);
        let tree = GT.create(state);
        OUnit.assert_equal(
          [(Action.Skip, GS.rotate_to_next_player(state))],
          List.map(((a, t)) => (a, GT.get_state(t))) @@
          GT.get_subtrees(tree),
        );
      ),
      OUnit2.(>::)("test_get_subtrees", _ =>
        /* <red>   <0,1>   <0,2>
         *     <--->   <bro>   <1,2> */
        let red_pos = {Pos.row: 0, col: 0};
        let brown_pos = {Pos.row: 1, col: 1};
        let hole_pos = {Pos.row: 1, col: 0};
        let pos01 = {Pos.row: 0, col: 1};
        let pos02 = {Pos.row: 0, col: 2};
        let conf =
          Conf.create(~width=3, ~height=2)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes([hole_pos]);
        let board = B.create(conf);
        let colors = [Color.Brown, Color.Red, Color.Black];
        let state = GS.create(board, colors);
        let state = GS.place_penguin(state, Color.Red, red_pos);
        let state = GS.place_penguin(state, Color.Brown, brown_pos);
        let tree = GT.create(state);
        let act_state_pairs =
          List.map(((a, t)) => (a, GT.get_state(t))) @@
          GT.get_subtrees(tree);

        OUnit.assert_equal(2) @@ List.length(act_state_pairs);
        OUnit.assert_equal(
          Some(
            GS.rotate_to_next_player @@
            GS.move_penguin(state, brown_pos, pos01),
          ),
          List.assoc_opt(
            Action.Move({Action.Move.src: brown_pos, dst: pos01}),
            act_state_pairs,
          ),
        );
        OUnit.assert_equal(
          Some(
            GS.rotate_to_next_player @@
            GS.move_penguin(state, brown_pos, pos02),
          ),
          List.assoc_opt(
            Action.Move({Action.Move.src: brown_pos, dst: pos02}),
            act_state_pairs,
          ),
        );
      ),
    ],
  );

let _ = OUnit2.run_test_tt_main(tests);
