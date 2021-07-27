module T = Fish.Common.Tile;

let tests =
  OUnit2.(>:::)(
    "tile_tests",
    [
      OUnit2.(>::)("test_construction", _ =>
        let t3 = T.create(3);
        OUnit2.assert_equal(3, T.get_fish(t3));
        OUnit2.assert_equal(false, T.is_hole(t3));
      ),
      OUnit2.(>::)("test_hole", _ =>
        let hole = T.hole;
        OUnit2.assert_equal(0, T.get_fish(hole));
        OUnit2.assert_equal(true, T.is_hole(hole));
      ),
      OUnit2.(>::)("test_negative_fish_count", _ =>
        let expect = Failure("fish count must be positive");
        OUnit2.assert_raises(expect, () => T.create(0));
        OUnit2.assert_raises(expect, () => T.create(- 1));
      ),
    ],
  );

let _ = OUnit2.run_test_tt_main(tests);
