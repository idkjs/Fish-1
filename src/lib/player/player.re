open! Core;
module Strategy = Strategy;
open Strategy;

module PS = Player_state;
module GS = Game_state;

class virtual t (name: string, age: int) = {
  as _;
  pub virtual place_penguin: Game_state.t => option(Position.t);
  pub virtual take_turn: Game_tree.t => option(Action.t);
  pub get_name = () => name;
  pub get_age = () => age;
  pub inform_tournament_start = () => true;
  pub assign_color = (_: PS.Player_color.t) => true;
  pub inform_disqualified = () => true;
  pub inform_tournament_result = (_: bool) => true;
  pub dispose = () => ();
};

class ai_player
      (name: string, age: int, placer: Penguin_placer.t, actor: Turn_actor.t) = {
  as _;
  inherit (class t)(name, age);
  val placer = placer;
  val actor = actor;
  pub place_penguin = gs => Option.some @@ Penguin_placer.use(placer, gs);
  pub take_turn = gt => Option.some @@ Turn_actor.use(actor, gt);
};

let create_AI_player = (~name="AI", ~age=0, placer, actor) =>
  (new ai_player)(name, age, placer, actor);
