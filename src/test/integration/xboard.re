open! Core;
module S = Fish.Serialize.Serialization;
module B = Fish.Common.Board;

/* deserialize a board-posn object from stdin, and output the # of reachable
 * positions starting from that position on the board */
let () = {
  let input = Core.In_channel.input_all(Core.In_channel.stdin);
  let serialized =
    S.deserialize(input)
    |> Result.of_option(~error="invalid serialization form");
  switch (Result.bind(~f=S.to_board_posn, serialized)) {
  | Error(reason) => Printf.printf("Invalid input, reason: %s\n", reason)
  | [@implicit_arity] Ok(board, pos) =>
    let reachable_count =
      List.fold_left(~init=0, ~f=(n, (_, dsts)) => n + List.length(dsts)) @@
      B.get_reachable_from(board, pos);
    Printf.printf("%d\n", reachable_count);
  };
};
