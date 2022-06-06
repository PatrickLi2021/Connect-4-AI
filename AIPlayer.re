open! CS17SetupGame;
open Game;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;
  open PlayerGame;
  /*
    Data Definition:
    A list('a) is one of:
      - []
      - [a, ...b] where a is a 'a, and b is a list('a).
   
  Inputs:
     - lst, a list('a)
     - f, a procedure that takes 'a and produces a float
   
  Outputs: A ('a, float) pair, where the first is the element in lst that
           returns the greatest value when f is applied to it, out of all the 
           elements in the list, and the second is that greatest f value.

  Recursion Diagrams:
   
   Original Input:  [1.0, 2.0, 3.0], x => x *. x
   Recursive Input: [2.0, 3.0], x => x *. x
   
   Ideation Space: To see if the first element returns the greatest value, 
                   the procedure has to be able to compare it to something.
                   so it should get the element that returns the greatest 
                   value in the rest of the list, and then compare it to the 
                   first element in the OI. 
                   --> RI should be tl and the same procedure.
                   --> compare the result of applying the procedure to the
                   first element in the OI list, to the first item in the 
                   RO pair. If the item from the RO pair is greater, output the 
                   RO. If not, output the pair that contains the first element 
                   in the OI list and the result of applying the procedure to 
                   that element. base case??
   
   
   Recursive Output: (3.0, 9.0)
   Overall Output: (3.0, 9.0)


   Original Input: [4.0, 3.0], x => x +. x
   Recursive Input: [3.0], x => x +. x

   Ideation Space: If there is one element in the input list, there is nothing
                   to compare it to, so just return the pair containing that 
                   sole element and the result of applying the procedure to 
                   that element. this means that if the list is empty, 
                   the program should error. Then the idea from the first 
                   recursion diagram works. Apply the procedure to the first 
                   element in the OI list, and compare it with the second 
                   element in the RO pair. since 8.0 is greater than 6.0, 
                   return a pair containinig the first element in the list 
                   and the result of applying the procedure to that element 
                   (which is (4.0, 8.0)).

   Recursive Output: (3.0, 6.0)
   Overall Outpu: (4.0, 8.0)
  */
  
  let rec argMax: (list('a), 'a => float) => ('a, float) =
    (lst, f) =>
      switch (lst) {
      | [] => failwith("domain error")
      | [a] => (a, f(a))
      | [hd, ...tl] =>
        let (rn, rf) = argMax(tl, f);
        let hdf = f(hd);
        if (rf > hdf) {
          (rn, rf);
        } else {
          (hd, hdf);
        };
      };
  
  /*
    Input:
      - lst, a list('a)
      - f, a procedure that takes 'a and produces a float
    
    Output: A ('a, float) pair, where the first is the element in lst that 
            returns the smallest value when f is applied to it, out of all 
            the elements in the list, and the second is that smallest f value.
  */
  
  let argMin: (list('a), 'a => float) => ('a, float) =
    (lst, f) => {
      let fneg = x => -. f(x);
      argMax(lst, fneg);
    };
  
  /*
    Inputs:
      - s, a state
      - depth, an integer greater than or equal to 0 that represents the 
        number of moves that the AI player is looking ahead by, or the depth 
        it's looking ahead by in the 'game tree'
   
    Output: A float representing the value associated with the best next state, 
            from making the best move, for the current player, based on the 
            number of moves the player looks ahead by. the greatest value if 
            the current player is P1, and the smallest value if the current 
            player is P2.

  Recursion Diagrams:
   
  Original Input:  State([[E, B, R, B], [E, R, R, R]], Ongoing(P2)), 1
  Recursive Input: State([[R, B, R, B], [E, R, R, R]], Onging(P1)), 0 and 
                     State([[E, B, R, B], [R, R, R, R]], Win(P2)), 0
    
    
    Ideation Space:
      - First, if the depth is 0, then we are at the end of the depth we're 
        looking ahead by, so just use estimateValue.
      - Otherwise, we have to check for a win or a draw: if a win, return the 
        winning value for the respective players. If a draw, return 0.0 
        because it isn't good for anyone
      - If ongoing, we want to get a list of all of the possible next states
        from the current state. then for each of those possible next states,
        we want to apply minimax to find the best value associated with each 
        of those states --> recursive call with the inputs as each of those 
        next states and depth-1, since by going to the next state, we've gone 
        one more layer deep!
      - Applying minimax to each of the next states would return a list of 
        floats where each float is the RO of calling minimax recursively.
      - To find the final best value: if P2, apply argMin on the list of 
        floats to find the smallest value (good for P2) and the state 
        associated with it, and then just return that value? Works for this 
        example input: if it was Ongoing(P1), we would do the same thing but 
        with argMax at the end
      - Maybe also multiply the RO of minimax on each next state by 
        0.999999999999999 to diminish the value of "later" wins and 
        prioritize "earlier" wins (i.e. states that give wins without having 
        to go the entire layer deep?)

  Recursive Output: -9399.8 and -9.99000000000000111e+80
  Overall Output: -9.99000000000000111e+8
   

   Original Input:  State([[E, B, R, B], [E, R, R, R]], Ongoing(P1)), 1
   Recursive Input: State([[B, B, R, B], [E, R, R, R]], Ongoing(P2)) and
                    State([[E, B, R, B], [B, R, R, R]], Ongoing(P2))
   
   Ideation Space:
      - Ideas from above for depth 0, draw, and win states work
      - If ongoing, find all possible next states and call minimax on each of 
        those states with one less depth
      - Since it's P1's turn, apply argMax to a list of the RO to find the 
        next state that returns the biggest value (good for P1) and return 
        that value
      - Combining the first and second ideation spaces takes care of the 
        alternating minimaizing-maximizing because every time it recurs, 
        which player's turn it is changes and argMax/Min gets called.

   Recursive Output: -1.00000000000103e+16 and 99999999999800.4
   Original Output: 99999999999800.4
   

   */

  let rec minimax: (state, int) => float =
    (s, depth) =>
      switch (depth) {
      | 0 => estimateValue(s)
      | _ =>
        switch (gameStatus(s)) {
        | Draw => 0.0
        | Win(P1) => 9.99 *. 10.0 ** 80.0 /*  (9.99 * 10^80) */
        | Win(P2) => (-9.99) *. 10.0 ** 80.0 // (-9.99 * 10^80)
        | Ongoing(P1) =>
          let (_newState, value) =
            argMax(List.map(m => nextState(s, m), legalMoves(s)), st =>
              minimax(st, depth - 1)
            );
          0.999999999999999 *. value;
        | Ongoing(P2) =>
          let (_newState, value) =
            argMin(List.map(m => nextState(s, m), legalMoves(s)), st =>
              minimax(st, depth - 1)
            );
          0.999999999999999 *. (-. value);
        }
      };

  /*
      Input:  s, a state
      Output: The best move for the current player to make, based on
              the current state that they are in
  */
  
  let nextMove: state => move =
    s => {
      /*
        Input:
           - lstSt, a list of states representing all possible next states
             from the original state given to nextMove
           - st, a state representing the best next state that is associated 
             with the value found by minimax
           - n, an integer representing the location of the element in lstSt 
             that it is currently looking at
       
        Output: An integer representing the location of the state inside lstSt
                that corresponds to st
      */
      
      let rec moveLocationHelper: (list(state), state, int) => int =
        (lstSt, st, n) =>
          switch (lstSt) {
          | [] => failwith("")
          | [hd, ...tl] =>
            if (hd == st) {
              n;
            } else {
              moveLocationHelper(tl, st, n + 1);
            }
          };

      let lstMoves = legalMoves(s);
      let lstStates = List.map(m => nextState(s, m), lstMoves);
      if (gameStatus(s) == Ongoing(P1)) {
        let (bestState, _bestValue) =
          argMax(lstStates, st => minimax(st, 4));
        List.nth(lstMoves, moveLocationHelper(lstStates, bestState, 0));
      } else {
        let (bestState, _bestValue) =
          argMin(lstStates, st => minimax(st, 4));
        List.nth(lstMoves, moveLocationHelper(lstStates, bestState, 0));
      };
    };

  /* put your team name here! */
  let playerName = "Jean and Patrick";
};

module TestGame = Connect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame);
module MyAIPlayer: Player = TestAIPlayer /* Test Cases for nextMove*/;

open TestAIPlayer;

// Test Cases for argMax:
checkExpect(
  argMax([1.0, 2.0, 3.0, 4.0], x => x +. 2.0),
  (4., 6.),
  "list of floats",
);
checkExpect(
  argMax([15.7, 66.12, (-6.23), 77.2], x => x +. 5.5),
  (77.2, 82.7),
  "list of floats with neg.",
);
checkExpect(argMax([5.0], x => x *. 5.5), (5., 27.5), "one element list");

// Test Cases for argMin:
checkExpect(
  argMin([1.6, 2.12, 3.1, (-6.55)], x => x -. 2.0),
  ((-6.55), 8.55),
  "list of floats",
);
checkExpect(
  argMin([15.44, 62.122, (-100.0), 100.0], x => x /. 5.0),
  ((-100.), 20.),
  "list of floats with neg.",
);
checkExpect(
  argMin([5.0, 3.2], x => x *. 5.5),
  (3.2, (-17.6)),
  "operator is multiplication",
);