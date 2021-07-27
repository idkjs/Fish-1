module P = Fish.Common.Player_state;
module PC = Fish.Common.Player_state.Player_color;
module PN = Fish.Common.Penguin;
module Pos = Fish.Util.Position;

let tests =
  OUnit2.(>:::)(
    "player_state_tests",
    [
      OUnit2.(>::)("test_construction", _ =>{
        let p = P.create(PC.Black);
        OUnit2.assert_equal(PC.Black, P.get_color(p));
        OUnit2.assert_equal(0, P.get_score(p));
        OUnit2.assert_equal([], P.get_penguins(p));
      }),
      OUnit2.(>::)("test_set_score", _ =>{
        /* 1. only penguin positions are updated, correctly
         * 2. no side effect
         * 3. errors on bad input */
        let p1 = P.create(PC.Black);
        let p2 = P.set_score(p1, 42);
        OUnit2.assert_equal(PC.Black, P.get_color(p1));
        OUnit2.assert_equal(0, P.get_score(p1));
        OUnit2.assert_equal([], P.get_penguins(p1));
        OUnit2.assert_equal(PC.Black, P.get_color(p2));
        OUnit2.assert_equal(42, P.get_score(p2));
        OUnit2.assert_equal([], P.get_penguins(p2));
        let expect = Failure("score must be non-negative");
        OUnit2.assert_raises(expect, () => P.set_score(p1, - 1));
      }),
      OUnit2.(>::)("test_add_penguin", _ =>{
        /* 1. only penguin positions are updated, correctly
         * 2. no side effect */
        let p1 = P.create(PC.Black);
        let pos = {Pos.row: 3, col: 5};
        let p2 = P.add_penguin(p1) @@ PN.create(pos);
        OUnit2.assert_equal(PC.Black, P.get_color(p1));
        OUnit2.assert_equal(0, P.get_score(p1));
        OUnit2.assert_equal([], P.get_penguins(p1));
        OUnit2.assert_equal(PC.Black, P.get_color(p2));
        OUnit2.assert_equal(0, P.get_score(p2));
        OUnit2.assert_equal([PN.create(pos)], P.get_penguins(p2));
      }),
      OUnit2.(>::)("test_move_penguin", _ =>{
        /* 1. only penguin positions and score are updated, correctly
         * 2. no side effect
         * 3. return None when no penguin is at 1st position */
        let p1 = P.create(PC.Red);
        let src = {Pos.row: 3, col: 5};
        let dst = {Pos.row: 3, col: 3};
        let p2 = P.add_penguin(p1) @@ PN.create(src);
        let p3 = P.move_penguin(p2, src, dst) |> Option.get;
        OUnit2.assert_equal(PC.Red, P.get_color(p2));
        OUnit2.assert_equal(0, P.get_score(p2));
        OUnit2.assert_equal([PN.create(src)], P.get_penguins(p2));
        OUnit2.assert_equal(PC.Red, P.get_color(p3));
        OUnit2.assert_equal(0, P.get_score(p3));
        OUnit2.assert_equal([PN.create(dst)], P.get_penguins(p3));
        OUnit2.assert_equal(None) @@ P.move_penguin(p3, src, dst);
      }),
      OUnit2.(>::)("test_penguin_order", _ =>{
        /* order of penguins should be preserved by the time they were added. */
        let p1 = P.create(PC.White);
        let pos11 = {Pos.row: 1, col: 1};
        let pos23 = {Pos.row: 2, col: 3};
        let pos34 = {Pos.row: 3, col: 4};
        let pos77 = {Pos.row: 7, col: 7};
        let p2 = P.add_penguin(p1) @@ PN.create(pos11);
        let p3 = P.add_penguin(p2) @@ PN.create(pos23);
        let p4 = P.add_penguin(p3) @@ PN.create(pos34);
        OUnit2.assert_equal(PC.White, P.get_color(p4));
        OUnit2.assert_equal(0, P.get_score(p4));
        OUnit2.assert_equal(
          List.map(PN.create, [pos34, pos23, pos11]),
          P.get_penguins(p4),
        );
        let p5 = P.move_penguin(p4, pos11, pos77) |> Option.get;
        OUnit2.assert_equal(PC.White, P.get_color(p5));
        OUnit2.assert_equal(0, P.get_score(p5));
        OUnit2.assert_equal(
          List.map(PN.create, [pos34, pos23, pos77]),
          P.get_penguins(p5),
        );
      }),
    ],
  );

let _ = OUnit2.run_test_tt_main(tests);
