/** Uses Ounit to check whether given 2 list of players have the same set of
    names, i.e., order is irrelevant. First list is expected. */

let check_same_set_of_players_by_names:
  (list(Fish.Player.t), list(Fish.Player.t)) => unit;

/** Given integer is used as its name. Default search depth is 1 */

let get_default_ai_player: (~depth: int=?, int) => Fish.Player.t;
