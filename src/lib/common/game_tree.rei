/** A [t] represents the root node of a (sub) game tree.
    It includes:
      - a game state.
      - subsequent game states and the actions to get there (lazily generated)
    NOTE that it's immutable */

type t;

/** Create a game tree starting from the given state */

let create: Game_state.t => t;

let get_state: t => Game_state.t;

/** Return a list that associates each of the legal action from current player
    in the game state to the resulting state from that action.
    If current player cannot make a move, but some other player can, the only
    action returned is [Skip], associated with a game state whose current player
    is rotated to the next one.
    Returning an empty list means no player can make a legal move anymore. */

let get_subtrees: t => list((Action.t, t));
