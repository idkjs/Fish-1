open! Core;
module S = Fish.Serialize.Serialization;
module B = Fish.Common.Board;
module Action = Fish.Common.Action;
module Move = Fish.Common.Action.Move;
module Game_tree = Fish.Common.Game_tree;
module GS = Fish.Common.Game_state;
module Pos = Fish.Util.Position;

module Conf = Fish.Common.Board.Config;
module Ref = Fish.Admin.Referee;

/** deserialize a move_response_query object, and print out the serialized form
    of selected move from [select_next_move_if_possible], or print "false" if
    desired move isn't possible. */

let () = {
  let input = Core.In_channel.input_all(Core.In_channel.stdin);
  let serialized =
    S.deserialize(input)
    |> Result.of_option(~error="invalid serialization form");
  switch (Result.bind(~f=S.to_game_description, serialized)) {
  | Error(reason) => Printf.printf("Invalid input, reason: %s\n", reason)
  | [@implicit_arity] Ok(height, width, players, fish) =>
    let conf =
      Conf.create(~width, ~height)
      |> Conf.set_min_num_of_one_fish_tile(0)
      |> Conf.set_holes([])
      |> Conf.set_default_num_of_fish(fish);
    let winners =
      Ref.run_game(Ref.create(), players, conf).winners
      |> List.map(~f=p => p#get_name())
      |> List.sort(~compare=String.compare);
    S.from_list(winners, S.from_string)
    |> S.serialize
    |> Printf.printf("%s\n");
  };
};
