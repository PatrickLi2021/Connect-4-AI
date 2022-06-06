module type Game = {
  /*
      Data Definition:
      The type whichPlayer represents the players playing the game. Player 1 is
      represented as P1, and player 2 is represented as P2.

      Example Data:
      P1, P2
   */

  type whichPlayer =
    | P1
    | P2;

  /*
      Data Definition:
      The type status represents whether the game has been won
      (and by which player), ended in a draw, or is ongoing (and whose turn it
      is), represented respectively as one of:
         - Win(whichPlayer)
         - Draw
         - Ongoing(whichPlayer)

      Example Data:
      Win(P1), Win(P2), Draw, Ongoing(P1), Ongoing(P2)
   */

  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);

  /*
    Data Definition:
    The type state represents the state of the Connect4 game, containing
    information about what the board looks like and the status of the
    game (whether a player has won, or the game ended in a draw, or
    is still ongoing), as State(board, status).

    Example Data:
    State([E, E, E, E], [E, E, E, B], [E, E, E, R], [E, E, E, B], Ongoing(P2)),
    State([B, R, B, R], [R, R, R, R], [B, B, B, B], [R, R, R, R], Draw)
    State([E, E, R, R, B], [E, E, E, E, B], [E, R, R, R, B], [E, E, E, E, B],
           Win(P1))
   */

  type state;

  /*
       Data Definition:
       The type move represents a move made in the Connect4 game as Move(int),
       where the integer is a positive integer representing the number label of
       the column that the player chose to drop their game piece into; Move(1)
       would indicate that the player chose to drop their piece into the first
       column on the board.

       Example Data:
       Move(1), Move(2), Move(5)
   */

  type move;

  /* printing functions */
  let stringOfPlayer: whichPlayer => string;
  let stringOfState: state => string;
  let stringOfMove: move => string;

  /* Game Logic */

  /* the state of the game when it begins */
  let initialState: string => state;

  /* produces the list of legal moves at a state */
  let legalMoves: state => list(move);

  /* returns the status of the game at the given state */
  let gameStatus: state => status;

  /* given a state and a legal move, yields the next state */
  let nextState: (state, move) => state;

  /* for transforming human player input into
     internal representation of move */
  let moveOfString: (string, state) => move;

  /* estimates the value of a given state (static evaluation) */
  let estimateValue: state => float;
};