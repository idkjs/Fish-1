module Pos = Fish.Util.Position;
module B = Fish.Common.Board;
module Conf = Fish.Common.Board.Config;
module T = Fish.Common.Tile;

let board_tests =
  OUnit2.(>:::)(
    "board_tests",
    [
      OUnit2.(>::)("test_construction", _ => {
        let (width, height) = (3, 4);
        let holes = [{Pos.row: 2, col: 2}, {Pos.row: 0, col: 0}];
        let min_one_fish_tile = 3;
        let dflt_fish = 5;
        let conf =
          Conf.create(~height, ~width)
          |> Conf.set_holes(holes)
          |> Conf.set_min_num_of_one_fish_tile(min_one_fish_tile)
          |> Conf.set_default_num_of_fish(dflt_fish);

        let board = B.create(conf);
        OUnit2.assert_equal(width, B.get_width(board));
        OUnit2.assert_equal(height, B.get_height(board));
        let fishes: list(int) = (
          Pos.create_positions_within(~width, ~height)
          |> List.map(pos => {
               let tile = B.get_tile_at(board, pos);
               if (List.mem(pos, holes)) {
                 let _ = OUnit2.assert_equal(true, T.is_hole(tile));
                 0;
               } else {
                 let _ = OUnit2.assert_equal(false, T.is_hole(tile));
                 let fish = T.get_fish(tile);
                 let _ =
                   OUnit2.assert_equal(true, fish == dflt_fish || fish == 1);
                 fish;
               };
             }):
            list(int)
        );

        let one_fish_tiles =
          List.filter(fish => fish == 1, fishes) |> List.length;

        OUnit2.assert_equal(true, one_fish_tiles >= min_one_fish_tile);

        /* non-positive dimension */
        let expect = Failure("Board dimension must be positive");
        OUnit2.assert_raises(expect, () =>
          B.create @@ Conf.set_width(0, conf)
        );
        OUnit2.assert_raises(expect, () =>
          B.create @@ Conf.set_height(0, conf)
        );
        OUnit2.assert_raises(expect, () =>
          B.create @@ Conf.set_width(-2, conf)
        );
        OUnit2.assert_raises(expect, () =>
          B.create @@ Conf.set_height(-1, conf)
        );

        /* within board */
        OUnit.assert_equal(true) @@
        B.within_board(board, {Pos.row: 3, col: 2});
        OUnit.assert_equal(false) @@
        B.within_board(board, {Pos.row: 3, col: 3});
        OUnit.assert_equal(false) @@
        B.within_board(board, {Pos.row: 4, col: 2});
        OUnit.assert_equal(true) @@
        B.within_board(board, {Pos.row: 0, col: 0});
        OUnit.assert_equal(false) @@
        B.within_board(board, {Pos.row: (-1), col: 0});
        OUnit.assert_equal(false) @@
        B.within_board(board, {Pos.row: 0, col: (-1)});
      }),
      OUnit2.(>::)("test_remove_tile_at", _ => {
        /* 1. removes correct tile
         * 2. errors if pos is out of bound */
        let (width, height) = (3, 4);
        let holes = [];
        let min_one_fish_tile = 0;
        let dflt_fish = 3;
        let conf =
          Conf.create(~height, ~width)
          |> Conf.set_holes(holes)
          |> Conf.set_min_num_of_one_fish_tile(min_one_fish_tile)
          |> Conf.set_default_num_of_fish(dflt_fish);

        let board = B.create(conf);
        let pos11 = {Pos.row: 1, col: 1};

        let board = B.remove_tile_at(board, pos11);
        OUnit2.assert_equal(width, B.get_width(board));
        OUnit2.assert_equal(height, B.get_height(board));
        Pos.create_positions_within(~width, ~height)
        |> List.iter(pos => {
             let tile = B.get_tile_at(board, pos);
             if (pos == pos11) {
               OUnit2.assert_equal(true) @@ T.is_hole(tile);
             } else {
               OUnit2.assert_equal(false) @@ T.is_hole(tile);
             };
           });

        let pos33 = {Pos.row: 3, col: 3};
        let expect = Failure("Position is outside the board");
        OUnit2.assert_raises(expect, _ => B.get_tile_at(board, pos33));
        OUnit2.assert_raises(expect, _ => B.remove_tile_at(board, pos33));
      }),
      OUnit2.(>::)("test_get_copy", _ => {
        /* update original has no effect on copy */
        let (width, height) = (3, 4);
        let holes = [];
        let min_one_fish_tile = 0;
        let dflt_fish = 3;
        let conf =
          Conf.create(~height, ~width)
          |> Conf.set_holes(holes)
          |> Conf.set_min_num_of_one_fish_tile(min_one_fish_tile)
          |> Conf.set_default_num_of_fish(dflt_fish);

        let board = B.create(conf);
        let pos11 = {Pos.row: 1, col: 1};
        let copy = B.get_copy(board);
        let _ = B.remove_tile_at(board, pos11);
        Pos.create_positions_within(~width, ~height)
        |> List.iter(pos => {
             let tile = B.get_tile_at(copy, pos);
             OUnit2.assert_equal(3) @@ T.get_fish(tile);
           });
      }),
      /* (0, 0)  (0, 1)  (----)  (----)  (0, 4)
       *     (1, 0)  (1, 1)  (1, 2)  (1, 3)  (1, 4)
       * (2, 0)  (2, 1)  (2, 2)  (2, 3)  (2, 4)
       *     (3, 0)  (----)  (3, 2)  (3, 3)  (3, 4)
       * (4, 0)  (4, 1)  (4, 2)  (4, 3)  (4, 4) */
      OUnit2.(>::)("test_get_reachable_from", _ => {
        let (width, height) = (5, 5);
        let holes = [];
        let min_one_fish_tile = 0;
        let dflt_fish = 3;
        let conf =
          Conf.create(~height, ~width)
          |> Conf.set_holes(holes)
          |> Conf.set_min_num_of_one_fish_tile(min_one_fish_tile)
          |> Conf.set_default_num_of_fish(dflt_fish);

        let pos02 = {Pos.row: 0, col: 2};
        let pos03 = {Pos.row: 0, col: 3};
        let pos31 = {Pos.row: 3, col: 1};
        let board = B.create(conf);
        let board = B.remove_tile_at(board, pos02);
        let board = B.remove_tile_at(board, pos03);
        let board = B.remove_tile_at(board, pos31);
        /* [dir * [pos]] */
        let result = B.get_reachable_from(board, {Pos.row: 2, col: 2});
        let nn_pos = [];
        let ne_pos = [{Pos.row: 1, col: 2}];
        let nw_pos = [{Pos.row: 1, col: 1}, {Pos.row: 0, col: 1}];
        let ss_pos = [{Pos.row: 4, col: 2}];
        let se_pos = [{Pos.row: 3, col: 2}, {Pos.row: 4, col: 3}];
        let sw_pos = [];
        open B.Direction;
        OUnit2.assert_equal(nn_pos) @@ List.assoc(North, result);
        OUnit2.assert_equal(ne_pos) @@ List.assoc(Northeast, result);
        OUnit2.assert_equal(nw_pos) @@ List.assoc(Northwest, result);
        OUnit2.assert_equal(ss_pos) @@ List.assoc(South, result);
        OUnit2.assert_equal(se_pos) @@ List.assoc(Southeast, result);
        OUnit2.assert_equal(sw_pos) @@ List.assoc(Southwest, result);
      }),
    ],
  );

let config_tests =
  OUnit2.(>:::)(
    "board_config_tests",
    [
      OUnit2.(>::)("test_construction", _ => {
        let conf = Conf.create(~width=3, ~height=4);
        OUnit2.assert_equal(3) @@ Conf.get_width(conf);
        OUnit2.assert_equal(4) @@ Conf.get_height(conf);
        OUnit2.assert_equal([]) @@ Conf.get_holes(conf);
        OUnit2.assert_equal(0) @@ Conf.get_min_num_of_one_fish_tile(conf);
        OUnit2.assert_equal(1) @@ Conf.get_default_num_of_fish(conf);
      }),
      OUnit2.(>::)("test_setter_getter", _ => {
        /* Only 1 field changes */
        let conf = Conf.create(~width=3, ~height=5);

        let conf1 = Conf.set_width(10, conf);
        OUnit2.assert_equal(10) @@ Conf.get_width(conf1);
        OUnit2.assert_equal(5) @@ Conf.get_height(conf1);
        OUnit2.assert_equal([]) @@ Conf.get_holes(conf1);
        OUnit2.assert_equal(0) @@ Conf.get_min_num_of_one_fish_tile(conf1);
        OUnit2.assert_equal(1) @@ Conf.get_default_num_of_fish(conf1);

        let conf1 = Conf.set_height(8, conf);
        OUnit2.assert_equal(3) @@ Conf.get_width(conf1);
        OUnit2.assert_equal(8) @@ Conf.get_height(conf1);
        OUnit2.assert_equal([]) @@ Conf.get_holes(conf1);
        OUnit2.assert_equal(0) @@ Conf.get_min_num_of_one_fish_tile(conf1);
        OUnit2.assert_equal(1) @@ Conf.get_default_num_of_fish(conf1);

        let conf1 = Conf.set_min_num_of_one_fish_tile(7, conf);
        OUnit2.assert_equal(3) @@ Conf.get_width(conf1);
        OUnit2.assert_equal(5) @@ Conf.get_height(conf1);
        OUnit2.assert_equal([]) @@ Conf.get_holes(conf1);
        OUnit2.assert_equal(7) @@ Conf.get_min_num_of_one_fish_tile(conf1);
        OUnit2.assert_equal(1) @@ Conf.get_default_num_of_fish(conf1);

        let conf1 = Conf.set_default_num_of_fish(3, conf);
        OUnit2.assert_equal(3) @@ Conf.get_width(conf1);
        OUnit2.assert_equal(5) @@ Conf.get_height(conf1);
        OUnit2.assert_equal([]) @@ Conf.get_holes(conf1);
        OUnit2.assert_equal(0) @@ Conf.get_min_num_of_one_fish_tile(conf1);
        OUnit2.assert_equal(3) @@ Conf.get_default_num_of_fish(conf1);

        let holes = [
          {Pos.row: 0, col: 2},
          {Pos.row: 3, col: 1},
          {Pos.row: 4, col: 5},
          {Pos.row: 1, col: 0},
        ];

        let conf1 = Conf.set_holes(holes, conf);
        OUnit2.assert_equal(3) @@ Conf.get_width(conf1);
        OUnit2.assert_equal(5) @@ Conf.get_height(conf1);
        OUnit2.assert_equal(holes) @@ Conf.get_holes(conf1);
        OUnit2.assert_equal(0) @@ Conf.get_min_num_of_one_fish_tile(conf1);
        OUnit2.assert_equal(1) @@ Conf.get_default_num_of_fish(conf1);

        /* original conf is unaffected */
        OUnit2.assert_equal(3) @@ Conf.get_width(conf);
        OUnit2.assert_equal(5) @@ Conf.get_height(conf);
        OUnit2.assert_equal([]) @@ Conf.get_holes(conf);
        OUnit2.assert_equal(0) @@ Conf.get_min_num_of_one_fish_tile(conf);
        OUnit2.assert_equal(1) @@ Conf.get_default_num_of_fish(conf);
      }),
    ],
  );

let _ = {
  OUnit2.run_test_tt_main(config_tests);
  OUnit2.run_test_tt_main(board_tests);
};
