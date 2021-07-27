module GT = Fish.Common.Game_tree;
module GS = Fish.Common.Game_state;
module B = Fish.Common.Board;
module Conf = Fish.Common.Board.Config;
module Color = Fish.Common.Player_state.Player_color;
module Pos = Fish.Util.Position;
module ST = Fish.Player.Strategy;
module Action = Fish.Common.Action;
module Move = Fish.Common.Action.Move;

let tests =
  OUnit2.(>:::)(
    "strategy_test",
    [
      OUnit2.(>::)("test_place_penguin_scanning", _ => {
        /* <0,0>   <hol>   <hol>
         *     <hol>   <1,1>   <1,2> */
        let holes = [
          {Pos.row: 0, col: 1},
          {Pos.row: 0, col: 2},
          {Pos.row: 1, col: 0},
        ];
        let conf =
          Conf.create(~width=3, ~height=2)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes(holes);
        let board = B.create(conf);
        let colors = [Color.Black, Color.Brown, Color.Red];
        let st = ST.Penguin_placer.create_scanning_strategy;

        let state = GS.create(board, colors);
        let pos00 = {Pos.row: 0, col: 0};
        OUnit.assert_equal(pos00) @@ ST.Penguin_placer.use(st, state);

        let state = GS.place_penguin(state, Color.Black, pos00);
        let pos11 = {Pos.row: 1, col: 1};
        OUnit.assert_equal(pos11) @@ ST.Penguin_placer.use(st, state);

        let state = GS.place_penguin(state, Color.Black, pos11);
        let pos12 = {Pos.row: 1, col: 2};
        OUnit.assert_equal(pos12) @@ ST.Penguin_placer.use(st, state);

        let state = GS.place_penguin(state, Color.Black, pos12);
        let expect = Failure("No position to place penguin on board");
        OUnit2.assert_raises(expect, _ => ST.Penguin_placer.use(st, state));
      }),
      OUnit2.(>::)("test_turn_action_minimax_skip", _ => {
        /* <red>   <hol>   <0,2>
         *     <hol>   <blk>   <1,2> */
        let holes = [{Pos.row: 0, col: 1}, {Pos.row: 1, col: 0}];
        let conf =
          Conf.create(~width=3, ~height=2)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes(holes);
        let board = B.create(conf);
        let colors = [Color.Red, Color.Brown, Color.Black];
        let st = ST.Turn_actor.create_minimax_strategy(3);
        let gs = GS.create(board, colors);
        let gs = GS.place_penguin(gs, Color.Red, {Pos.row: 0, col: 0});
        let gs = GS.place_penguin(gs, Color.Black, {Pos.row: 1, col: 1});

        OUnit.assert_equal(Action.Skip) @@
        ST.Turn_actor.use(st) @@
        GT.create(gs);
      }),
      OUnit2.(>::)("test_turn_action_minimax_move", _ => {
        /* <hol>   <0,1>   <0,2>
         *     <red>   <1,1>   <1,2>
         * <2,0>   <2,1>   <wit> */
        let holes = [{Pos.row: 0, col: 0}];
        let conf =
          Conf.create(~width=3, ~height=3)
          |> Conf.set_default_num_of_fish(3)
          |> Conf.set_holes(holes);
        let board = B.create(conf);
        let colors = [Color.Red, Color.White];
        let gs = GS.create(board, colors);
        let red_pos = {Pos.row: 1, col: 0};
        let white_pos = {Pos.row: 2, col: 2};
        let gs = GS.place_penguin(gs, Color.Red, red_pos);
        let gs = GS.place_penguin(gs, Color.White, white_pos);

        /* Looking ahead 1 turns
         * - red goes to (0, 1) by since it's more to the top left
         * - white goes to (0, 1) by since it's more to the top left */
        let st = ST.Turn_actor.create_minimax_strategy(1);
        OUnit.assert_equal(
          ~printer=Action.show,
          Action.Move({
            Move.src: red_pos,
            dst: {
              Pos.row: 0,
              col: 1,
            },
          }),
          ST.Turn_actor.use(st) @@ GT.create(gs),
        );
        OUnit.assert_equal(
          ~printer=Action.show,
          Action.Move({
            Move.src: white_pos,
            dst: {
              Pos.row: 0,
              col: 1,
            },
          }),
          ST.Turn_actor.use(st) @@ GT.create @@ GS.rotate_to_next_player(gs),
        );

        let gs =
          GS.move_penguin(gs, red_pos, {Pos.row: 0, col: 1})
          |> GS.rotate_to_next_player;
        /* <hol>   <red>   <0,2>
         *     <--->   <1,1>   <1,2>
         * <2,0>   <2,1>   <wit>
         *
         * Looking ahead 2 turns
         * - white goes to (0, 2) by since
         *   - either move allows 1 more move
         *   - (0, 2) more to the top left */
        let st = ST.Turn_actor.create_minimax_strategy(2);
        OUnit.assert_equal(
          ~printer=Action.show,
          Action.Move({
            Move.src: white_pos,
            dst: {
              Pos.row: 0,
              col: 2,
            },
          }),
          ST.Turn_actor.use(st) @@ GT.create(gs),
        );
        /* Looking ahead 3 turns
         * - white goes to (1, 1) by since
         *   - it's the only move that allows white to move 2 more times */
        let st = ST.Turn_actor.create_minimax_strategy(3);
        OUnit.assert_equal(
          ~printer=Action.show,
          Action.Move({
            Move.src: white_pos,
            dst: {
              Pos.row: 1,
              col: 1,
            },
          }),
          ST.Turn_actor.use(st) @@ GT.create(gs),
        );
      }),
    ],
  );
let _ = OUnit2.run_test_tt_main(tests);
