/* This file serves both as an interface for the player, and package boundary
 * for the player package. */
module Strategy = Strategy;

/** A [t] represents an external player in a fish game. It's "external" because
    it holds little information about a player's state in a fish game.
    It's responsible for actions from a player, either taking turns or
    responding to certain game events */

class virtual t:
  (string, int) =>
  {
    /** Assuming the game is in the initial penguin placement phase, return the
      position this player would like to place its next penguin
      Return [None] if there is a communication failure or player can't respond */
    pub virtual place_penguin: Game_state.t => option(Position.t);
    /** Assuming it's this player's turn, return the action it chooses to perform
      in the state within given game tree. It can also use the tree for planning
      purposes, and implicitly take advantage of subtree caching.
      Return [None] if there is a communication failure or player can't respond */
    pub virtual take_turn: Game_tree.t => option(Action.t);
    /** Return [true] if the player responded */
    pub inform_tournament_start: unit => bool;
    /** Assign given color to the given player. By default it does nothing.
      Return [true] if the player responded */
    pub assign_color: Player_state.Player_color.t => bool;
    /** Return the name associated with this player */
    pub get_name: unit => string;
    /** Return the age associated with this player.
      Players who signed up earlier should have smaller age. */
    pub get_age: unit => int;
    /** Inform this player that it has been disqualified from a fish game
      Return [true] if the player responded */
    pub inform_disqualified: unit => bool;
    /** Inform this player whether it has won the tournament
      Return [true] if the player responded */
    pub inform_tournament_result: bool => bool;
    /** Release all resources used by [t];
      [t] should never be used again after this call. */
    pub dispose: unit => unit;
  };

/** Create an AI player who uses given strategies for decision making.
    NOTE that it always respond on behalf of the current player in a game. */

let create_AI_player:
  (
    ~name: string=?,
    ~age: int=?,
    Strategy.Penguin_placer.t,
    Strategy.Turn_actor.t
  ) =>
  t;
