open Fish.Player.Strategy;
module Pos = Fish.Util.Position;
module Move = Fish.Common.Action.Move;
module Action = Fish.Common.Action;
open! Core;

/** A default well-behaving AI player which will be extended to create
    misbehaving mock players */
class ai_player (name) = {
  as _;
  inherit (class Fish.Player.t)(name, 42);
  val placer = Fish.Player.Strategy.Penguin_placer.create_scanning_strategy;
  val actor = Fish.Player.Strategy.Turn_actor.create_minimax_strategy(2);
  pub place_penguin = gs => Option.some @@ Penguin_placer.use(placer, gs);
  pub take_turn = gt => Option.some @@ Turn_actor.use(actor, gt);
};

/** Simulate indefinite hanging */

let block_for_a_long_time = (): 'a => {
  Unix.sleep(100000);
  failwith("Unreachable code");
};

let get_player_fail_at_placement = name => {
  as _;
  inherit (class ai_player)(name);
  pub! place_penguin = _ => None
};

let get_player_cheat_at_placement = name => {
  as _;
  inherit (class ai_player)(name);
  pub! place_penguin = _ => Some({Pos.row: (-1), col: 0})
};

let get_player_hang_at_placement = name => {
  as _;
  inherit (class ai_player)(name);
  pub! place_penguin = _ => block_for_a_long_time()
};

let get_player_fail_at_turn_action = name => {
  as _;
  inherit (class ai_player)(name);
  pub! take_turn = _ => None
};

let get_player_cheat_at_turn_action = name => {
  as _;
  inherit (class ai_player)(name);
  pub! take_turn = _ =>
    Some(
      Action.Move({
        Move.src: {
          Pos.row: 2,
          col: 2,
        },
        dst: {
          Pos.row: 0,
          col: (-1),
        },
      }),
    )
};

let get_player_hang_at_turn_action = name => {
  as _;
  inherit (class ai_player)(name);
  pub! take_turn = _ => block_for_a_long_time()
};

let get_player_hang_at_color_assignment = name => {
  as _;
  inherit (class ai_player)(name);
  pub! assign_color = _ => block_for_a_long_time()
};

let get_player_hang_at_inform_tournament_start = name => {
  as _;
  inherit (class ai_player)(name);
  pub! inform_tournament_start = block_for_a_long_time()
};

let get_player_hang_at_inform_tournament_result = name => {
  as _;
  inherit (class ai_player)(name);
  pub! inform_tournament_result = _ => block_for_a_long_time()
};

let get_player_hang_at_color_assignment_and_disqualification = name => {
  as _;
  inherit (class ai_player)(name);
  pub! assign_color = _ => block_for_a_long_time();
  pub! inform_disqualified = () => block_for_a_long_time()
};
