open CS17SetupGame;
open Game;

module Connect4 = {
  /*
     Data Definition:
     A list('a) is one of:
       - []
       - [a, ...b] where a is a 'a, and b is a list('a).
   */

  type whichPlayer =
    | P1
    | P2;

  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);

  type move =
    | Move(int);

  /*
   Data Definition:
   The type cell represents one game piece cell on the Connect4 board, and is
   one of:
      - E
      - R
      - B
      - G

   where E represents an empty cell on the board, R represents a cell
   occupied by the red chip (P2's color), B represents a cell occupied
   by the blue chip (P1's color), and G represents a bogus cell (which
   will be used when checking for four consecutive game pieces in a
   diagonal orientation).

   Example data:
   E, R, B, G
   */

  type cell =
    | E
    | R
    | B
    | G;

  /*
   Data Definition:
   A list(cell) is one of:
      - []
      - [a, ...b] where a is a cell, and b is a list(cell)
   A list(list(cell)) is one of:
      - []
      - [a, ...b] where a is a list(cell), and b is a list(list(cell))

   The type board represents a Connect4 game board and is a list(list(cell))
   with length that is at least 4, where each list(cell) also has length
   that is at least 4, and represents a column on the game board, so that
   board is a list of at least 4 columns.

   Example Data:
   list(cell):       [], [E], [R, B, B], [E, E, E, E], [E, R, B]
   list(list(cell)): [], [[E], [E]], [[E, R, R], [B, B, B]]
   board:            [[E, E, E, E], [E, E, E, E], [E, E, E, E], [E, E, E, E]],
                     [[E, B, B, R, B],[E, E, E, E, E], [B, B, R, B, R],
                      [E, B, B, R, R]]
   */

  type board = list(list(cell));

  type state =
    | State(board, status);

  /*
      Input:  An int, height, that represents how many slots to put in each
              column in the game board
      Output: A list of cells that contains the number of cells that was
              specified by the input int, height

      Recursion Diagrams:

      Original Input: 1
      Recursive Input: 0

      Ideation Space: If the recursive input is 0, then return a list
                      containing a single E as the overall output

      Recursive Output: N/A
      Overall Output: [E]

      Original Input: 3
      Recursive Input: 2

      Ideation Space: Cons an E onto the front of the recursive output

      Recursive Output: [E, E]
      Overall Output: [E, E, E]
   */

  let rec createInitialColumns: int => list(cell) =
    height =>
      switch (height) {
      | 0 => failwith("invalid dimensions")
      | 1 => [E]
      | h => [E, ...createInitialColumns(h - 1)]
      };

  /*
      Input:  An (int, int) pair that represents the height and the width of
              the game board to be created
      Output: A board that possesses the dimensions specified by the input int
              pair

      Recursion Diagrams:

      Original Input:  (5, 1)
      Recursive Input: (5, 0)

      Ideation Space: If the recursive input is 0, then return a list
                      containing a single E in it

      Recursive Output: N/A
      Overall Output:   [E]

      Original Input: (2, 2)
      Recursive Input: (2, 1)

      Ideation Space: Cons a list containing height empty cells onto the front
                      of the recursive output

      Recursive Output: [[E, E]]
      Overall Output: [[E, E], [E, E]]

   */

  let rec createInitialBoard: (int, int) => board =
    (height, width) =>
      switch (width) {
      | 0 => failwith("invalid dimensions")
      | 1 => [createInitialColumns(height)]
      | w => [
          createInitialColumns(height),
          ...createInitialBoard(height, w - 1),
        ]
      };

  /*
     Input:  A string, s, that represents the dimensions of the board on which
             the game will be played
     Output: A state that represents an empty Connect4 board for the game to be
             played on
   */

  let initialState: string => state =
    s => {
      let boardDims = parseBoardDims(s);
      let boardHeight = getBoardHeight(boardDims);
      let boardWidth = getBoardWidth(boardDims);
      State(createInitialBoard(boardHeight, boardWidth), Ongoing(P1));
    };

  /*
     Input:  A whichPlayer, player, that is either P1 or P2
     Output: A string that represents the player, either Player 1 or Player 2
   */

  let stringOfPlayer: whichPlayer => string =
    player =>
      switch (player) {
      | P1 => "Player 1"
      | P2 => "Player 2"
      };

  /*
     Input:  A cell that is either E, R, B, G (representing empty, red, or
             blue, or bogus)
     Output: A string representation of the input cell (either " " if the
             input was E, "R" for R, "B" for B, and "G" for G)
   */

  let stringOfCell: cell => string =
    c =>
      switch (c) {
      | E => " "
      | R => "R"
      | B => "B"
      | G => "G"
      };

  /*
      Input:  A list of strings, lst1 that represents a column of the
              game board
      Output: A string representation of the input column

      Recursion Diagrams:

      Original Input: ["E", "E", "E"]
      Recursive Input: ["E", "E"]

      Ideation Space: Add a string "E" to the front of the recursive output

      Recursive Output: "[E, E]"
      Overall Output: "[E, E, E]"

      Original Input: [E]
      Recursive Input: []

      Ideation Space: If the recursive input is an empty list, then return a
                      string representing a single cell, "[E]"

      Recursive Output: N/A (error)
      Overall Output: "[E]"
   */

  let rec stringOfColumn: list(string) => string =
    alos =>
      switch (alos) {
      | [] => failwith("Board cannot have an empty column")
      | [hd] => "[" ++ hd ++ "]"
      | [hd, ...tl] => "[" ++ hd ++ "] " ++ stringOfColumn(tl)
      };

  /*
      Input:  A board, b, that is a list of list of cells
      Output: A string that depicts the input board

      Recursion Diagrams:

      Original Input: []
      Recursive Input: N/A

      Ideation Space: If there is no recursive input (i.e. the original input
                      was an empty board), then return the string ""

      Recursive Output: N/A
      Overall Output: ""

      Original Input: [[E, E], [E, E], [E, E]]
      Recursive Input: [[E, E], [E, E]]

      Ideation Space: Add a list of E's (a column) to the recursive output
                      along with a line break

      Recursive Output: "[[E, E],
                          [E, E]]"
      Overall Output: "[[E, E]
                        [E, E],
                        [E, E]]"

   */

  let rec boardPrintHelper: board => string =
    lst2 =>
      switch (lst2) {
      | [] => ""
      | [hd, ...tl] =>
        stringOfColumn(List.map(stringOfCell, hd))
        ++ "\n"
        ++ boardPrintHelper(tl)
      };

  /*
     Input:  A board, bd, that is an internal representation of the
             current game state
     Output: A board that represents the input board but flipped, with the rows
             and columns switched

     Recursion Diagrams:

      Original Input: [[E, R], [R, E]]
      Recursive Input: [[R], [E]]

      Ideation Space: Add the heads of each original inner list as a list to
                      the recursive output

      Recursive Output: [[R, E]]
      Original Output: [[E, R], [R, E]]


      Original Input: [[E, B, R], [E, B, R]]
      Recursive Input: [[B, R], [B, R]]

      Ideation Space: Add the heads of each original inner list as a list to
                      the recursive output

      Recursive Output: [[B, B], [R, R]]
      Original Output: [[E, E], [B, B], [R, R]]
   */

  let rec transpose: board => board =
    bd =>
      switch (bd) {
      | []
      | [[], ..._] => failwith("a board cannot have an empty column")
      | [[_hd], ..._] => [List.flatten(bd)]
      | [[_hd, ..._tl], ..._] => [
          List.map(List.hd, bd),
          ...transpose(List.map(List.tl, bd)),
        ]
      };

  /*
      Input:  A state, st, that tells us the current situation of the game as
              well as whose turn it is to move
      Output: A string representation of the game board and which player's turn
              it is
   */

  let stringOfState: state => string =
    st =>
      switch (st) {
      | State(b, Win(_))
      | State(b, Draw)
      | State(b, Ongoing(_)) => boardPrintHelper(transpose(b))
      };

  /*
      Input:  A move, m, that represents which column the player wants to drop
              their piece into
      Output: A string representation of that move
   */

  let stringOfMove: move => string =
    m => {
      let Move(n) = m;
      string_of_int(n);
    };

  /*
      Input:  A state, st, that contains the current game board and the
              status of the game
      Output: A list of legal moves that can be played from the current
              configuration of the board
   */

  let legalMoves: state => list(move) =
    st => {
      /*
           Input:  A board, bd, that is an internal representation of the
                   current game board, and an int, n, that represents the
                   current column that the procedure is analyzing
           Output: A list of moves that can be played from the current game
                   state
       */

      let rec legalMoveHelper: (board, int) => list(move) =
        (bd, n) =>
          switch (bd) {
          | [] => []
          | [[], ..._tl2] => failwith("board column cannot be empty")
          | [[hd1, ..._tl1], ...tl2] =>
            if (hd1 == E) {
              [Move(n), ...legalMoveHelper(tl2, n + 1)];
            } else {
              legalMoveHelper(tl2, n + 1);
            }
          };

      switch (st) {
      | State(_, Win(_))
      | State(_, Draw) => []
      | State(b, Ongoing(_p)) => legalMoveHelper(b, 1)
      };
    };

  /*
     Input:  A state, st, that contains the current game board as well as the
             status of the game
     Output: A status that represents whether the game has been won or if it
             is ongoing
   */

  let gameStatus: state => status =
    st => {
      let State(_b, p) = st;
      p;
    };

  /*
     Input:  A string, str, representing the user's input and the current
             state of the game, st
     Output: The input string transformed into a type move, if it was one of
             the legal moves in the current game's state (st).
   */

  let moveOfString: (string, state) => move =
    (str, st) =>
      if (List.mem(str, List.map(stringOfMove, legalMoves(st)))) {
        Move(int_of_string(str));
      } else {
        failwith("Not a legal move; choose another move");
      };

  /*
     Input:  A board, bd, that is an internal representation of the game board
     Output: A new board that represents the input board but with the order of
             the inner lists of cells reversed
   */

  let horzFlip: board => board = bd => List.rev(bd);

  /*
     Input:  A board, bd, that is an internal representation of the current 
             game board
     Output: A new board that represents the input board but with the columns
             skewed so that the diagonals on the original board are now in line

   Recursion Diagrams:

   Original Input: [[R, R, R, R], [R, R, R, R], [R, R, R, R]]
   Recursive Input: [[R, R, R, R], [R, R, R, R]]

   Ideation Space: Call createBogusCells on the first sub-list of the board
                   and then cons it to the recursive output

   Recursive Output: [[G, R, R, R, R, G], [G, G, R, R, R, R]]
   Overall Output: [[R, R, R, R, G, G], [G, R, R, R, R, G], [G, G, R, R, R, R]]

   Original Input: [[R, R, R, B], [R, R, B, R], [R, B, R, R]]
   Recursive Input: [[R, R, B, R], [R, B, R, R]]

   Ideation Space: Call the createBogusCells procedure on the first sub-list
                   of the board and then cons it to the recursive output

   Recursive Output: [[G, R, R, B, R, G], [G, G, R, B, R, R]]
   Overall Output: [[R, R, R, B, G, G], [G, R, R, B, R, G], [G, G, R, B, R, R]]

   */

  let rec diagonalHelper: board => board =
    bd => {
      /*
         Input:  An int, n, that represents the number of "bogus cells" (cells
                 that contribute to the skew of a board on each end)
         Output: A list of cells with the "bogus cells" added to the end(s)
       */

      let rec createBogusCells: int => list(cell) =
        n =>
          switch (n) {
          | 0 => []
          | n => [G, ...createBogusCells(n - 1)]
          };

      switch (bd) {
      | [] => failwith("board should not have zero rows")
      | [[_hd, ..._tl]] => bd
      | [hd, ...tl] => [
          List.append(hd, createBogusCells(List.length([hd, ...tl]) - 1)),
          ...List.map(x => [G, ...x], diagonalHelper(tl)),
        ]
      };
    };

  /*
     Input:  A whichPlayer, p, a list of cells, col, and a list of moves, lst
     Output: A status that represents whether or not a player has won, the game
             is a draw, or if it is still ongoing.
   */

  let produceOutcome: (whichPlayer, list(cell), list(move)) => status =
    (p, col, lst) => {
      /*
         Input:  A list of cells, col, that represents the board
                 column that the procedure is checking
         Output: A boolean, true, that indicates if there is a 4 in a row in
                 the given input column, and false otherwise
       */

      let rec fourConsecutiveColumn: list(cell) => bool =
        col =>
          switch (col) {
          | [] => false
          | [B, B, B, B, ..._tl]
          | [R, R, R, R, ..._tl] => true
          | [_hd, ...tl] => fourConsecutiveColumn(tl)
          };

      if (fourConsecutiveColumn(col)) {
        Win(p);
      } else {
        switch (lst) {
        | [] => Draw
        | _ =>
          if (p == P1) {
            Ongoing(P2);
          } else {
            Ongoing(P1);
          }
        };
      };
    };

  /*
     Input:  A board, bd, a whichPlayer, p, and an int, n, that represent the
             current game board, the player whose turn it is, and the column
             that they want to drop their piece into
     Output: A new board that represents the updated board after the move was
             made


    Recursion Diagrams:

    Original Input: [[E, R, R, R], [B, B, R, B], [R, R, B, R], [B, R, B, R]],
    P1,
    0
    Recursive Input: [[E, R, R, R]]

    Ideation Space: The column/sub-list that the player wants to drop their
                    piece into is specified by the int in the original input
                    and then the helper procedure takes that specific column in
                    as its recursive input. If the player is P1, then a B is
                    added to the front of the list. If the player is P2, then
                    an R is added to the front of the list. We then cons the
                    rest of the list to the rest of the original input.

    Recursive Output:
    Overall Output: [[B, R, R, R]]

    Original Input: [[E, E, R, R], [E, B, B, B], [R, B, B, R], [B, B, B, R]],
    P2,
    1
    Recursive Input: [[E, B, B, B]]

    Ideation Space: The logic behind this example is similar to the above
                    recursion diagram: The specific inner list that the player
                    wants to drop their piece into is the recursive input and
                    their color replaces the first E in that column. That
                    column is then consed onto the rest of the original list.

    Recursive Output:
    Overall Output: [[E, E, R, R], [R, B, B, B], [R, B, B, R], [B, B, B, R]]

   */

  let rec changeBoardHelper: (board, whichPlayer, int) => board =
    (bd, p, n) => {
      /*
         Input:  A whichPlayer, p, and a list of cells, lst, for the player to
                 drop their piece into
         Output: A new list of cells that represents the specified column with
                 the most recent move made to it
       */

      let rec changeColumnHelper: (whichPlayer, list(cell)) => list(cell) =
        (p, lst) =>
          switch (lst) {
          | [E, E, ...tl] => [E, ...changeColumnHelper(p, [E, ...tl])]
          | [E, ...tl] =>
            if (p == P1) {
              [B, ...tl];
            } else {
              [R, ...tl];
            }
          | _ =>
            failwith(
              "Column should have at least 1 empty cell when the player"
              ++ " makes a move",
            )
          };

      switch (bd, n) {
      | ([hd, ...tl], 0) => [changeColumnHelper(p, hd), ...tl]
      | ([hd, ...tl], _) => [hd, ...changeBoardHelper(tl, p, n - 1)]
      | _ => failwith("board dimension and selected column number mismatch")
      };
    };

  /*
     Input:  A board, bd, and a whichPlayer, p, that represent the current
             game board and whose turn it is
     Output: A status that represents whether the game has been won by either
             player or if it is still ongoing
   */

  let checkFourInARow: (board, whichPlayer) => status =
    (bd, p) => {
      /*
         Input:  A whichPlayer, p, board, bd, and a list of statuses, lst
         Output: A list of statuses that contains the status of each move made
                 to the game board
       */

      let fourConsecutive: (whichPlayer, board, list(move)) => list(status) =
        (p, bd, lst) => List.map(x => produceOutcome(p, x, lst), bd);

      let lst = legalMoves(State(bd, Ongoing(p)));
      if (List.mem(
            Win(p),
            List.flatten(
              List.map(
                x => fourConsecutive(p, x, lst),
                [
                  bd,
                  transpose(bd),
                  transpose(diagonalHelper(transpose(bd))),
                  transpose(diagonalHelper(horzFlip(transpose(bd)))),
                ],
              ),
            ),
          )) {
        Win(p);
      } else {
        List.hd(fourConsecutive(p, bd, lst));
      };
    };

  /*
     Input:  A state, st, and a move, m, to be made on the board
     Output: A new state that contains a new board with the input move made on
             it
   */

  let nextState: (state, move) => state =
    (st, m) => {
      let Move(n) = m;
      switch (st) {
      | State(b, Ongoing(p)) =>
        let newBoard = changeBoardHelper(b, p, n - 1);
        State(newBoard, checkFourInARow(newBoard, p));
      | State(_b, _) =>
        failwith("Game should be ongoing when it reaches nextState")
      };
    };

  /*
     Input:  A list of cells that represents a column, row, or diagonal on the
             game board
     Output: A float that represents how good that row/column/diagonal
             configuration is for the player

     Recursion Diagrams:

     Original Input: [E, R, R, B, R]
     Recursive Input: [B, R]

     Ideation Space: Add result of evaluating first three elements of column
                     to the recursive output

     Recursive Output: 0.1
     Overall Output: -100.1

     Original Input: [R, R, R, B, R]
     Recursive Input: 100000000000000.0

     Ideation Space: Add result of evaluating first 4 elements of column to the
                     recursive output

     Recursive Output: -0.1
     Overall Output: 99999999999999.9
   */

  let rec pointsForXInAColumn: list(cell) => float =
    col =>
      switch (col) {
      | [] => 0.0
      | [_, B, B, B, B, ..._]
      | [B, B, B, B, ..._] => 9.99 *. 10.0 ** 80.0 /* 4 in a row for Blue */
      | [_, R, R, R, R, ..._]
      | [R, R, R, R, ..._] => (-9.99) *. 10.0 ** 80.0 /* 4 in a row for Red */
      | [E, B, B, B, E, ...tl] => 10000000.0 +. pointsForXInAColumn(tl)
      | [E, B, B, B, ...tl]
      | [R, B, B, E, B, ...tl]
      | [B, B, E, B, ...tl]
      | [B, E, B, B, ...tl]
      | [R, B, B, B, E, ...tl]
      | [B, B, B, E, ...tl] => 10000.0 +. pointsForXInAColumn(tl)
      /* 3 in a row for Blue */

      | [B, R, R, R, B, ...tl] =>
        1000000000000000.0 +. pointsForXInAColumn(tl)
      | [B, R, R, R, ...tl]
      | [R, R, B, R, ...tl]
      | [R, R, R, B, ...tl] => 100000000000000.0 +. pointsForXInAColumn(tl)
      /* Blocking Red from getting 4 in a row */

      | [R, B, B, B, R, ...tl] =>
        (-1000000000000000.0) +. pointsForXInAColumn(tl)
      | [R, R, B, ...tl]
      | [R, B, R, ...tl]
      | [B, R, R, ...tl] => 1000.0 +. pointsForXInAColumn(tl)
      /* Blocking red from getting 3 in a row */

      | [E, R, B, B, B, ...tl]
      | [R, B, B, B, ...tl]
      | [B, B, R, B, ...tl]
      | [B, B, B, R, ...tl] =>
        (-10000000000000000.0) +. pointsForXInAColumn(tl)
      /* Blocking Blue from getting 4 in a row */

      | [B, B, R, ...tl]
      | [B, R, B, ...tl]
      | [R, B, B, ...tl] => (-1000.0) +. pointsForXInAColumn(tl)
      /* Blocking blue from getting 3 in a row */

      | [E, R, R, R, E, ...tl] => (-10000000.0) +. pointsForXInAColumn(tl)
      | [R, R, R, E, ...tl]
      | [R, E, R, R, ...tl]
      | [R, R, E, R, ...tl]
      | [E, R, R, R, ...tl] => (-10000.0) +. pointsForXInAColumn(tl)
      /* 3 in a row for Red */

      | [B, E, B, ...tl]
      | [B, B, E, ...tl]
      | [E, B, B, ...tl]
      | [B, B, ...tl] => 100.0 +. pointsForXInAColumn(tl)
      /* 2 in a row for B */

      | [R, R, E, ...tl]
      | [R, E, R, ...tl]
      | [E, R, R, ...tl]
      | [R, R, ...tl] => (-100.0) +. pointsForXInAColumn(tl)
      /* 2 in a row for R */

      | [R, B, ...tl]
      | [B, R, ...tl]
      | [B, E, ...tl]
      | [E, B, ...tl]
      | [B, ...tl] => 0.1 +. pointsForXInAColumn(tl) /* 1 in a row for B */
      | [R, E, ...tl]
      | [E, R, ...tl]
      | [R, ...tl] => (-0.1) +. pointsForXInAColumn(tl) /* 1 in a row for R */
      | [_hd, ...tl] => pointsForXInAColumn(tl)
      };

  /*
     Input:  A board, bd, that is an internal representation of the current
             game board
     Output: A float that is the result of adding up all of the static
             evaluations of each individual row and column configuration for
             the normal board, vertically flipped board, horizontally flipped
             board, and the skewed board.
   */

  let totalPointsForState: board => float =
    bd =>
      List.fold_right(
        (x, y) => x +. y,
        List.map(
          x =>
            List.fold_right(
              (x, y) => x +. y,
              List.map(pointsForXInAColumn, x),
              0.0,
            ),
          [
            bd,
            transpose(bd),
            transpose(diagonalHelper(transpose(bd))),
            transpose(diagonalHelper(horzFlip(transpose(bd)))),
          ],
        ),
        0.0,
      );

  /*
     Input:  A state, st, that contains the current game board and status of
             the game
     Output: A float that quantifies the advantage for either P1 (Blue/B)
             or P2 (Red/R). If P1 has the advantage (or has already won), then
             the output float will be positive and if P2 has the advantage (or
             has already won), then the output float will be negative.
   */

  let estimateValue: state => float =
    st =>
      switch (st) {
      | State(bd, _status) => totalPointsForState(bd)
      };
};

module MyGame: Game = Connect4;
open Connect4;

// Test Cases/CheckErrors for createInitialColumns:
checkExpect(createInitialColumns(1), [E], "creates a column of length 1");
checkError(() => createInitialColumns(0), "invalid dimensions");
checkExpect(
  createInitialColumns(6),
  [E, E, E, E, E, E],
  "creates a column of length 6",
);
checkExpect(
  createInitialColumns(3),
  [E, E, E],
  "creates a column of length 1",
);

// Test Cases for createInitialBoard:
checkExpect(createInitialBoard(1, 1), [[E]], "1 by 1 board");
checkExpect(createInitialBoard(2, 1), [[E, E]], "2 by 1 board");
checkExpect(
  createInitialBoard(3, 3),
  [[E, E, E], [E, E, E], [E, E, E]],
  "3 by 3 board",
);
checkExpect(createInitialBoard(1, 3), [[E], [E], [E]], "1 by 3 board");

// Test Cases for initialState:
checkExpect(
  initialState("1 1"),
  State([[E]], Ongoing(P1)),
  "initialize a 1 by 1 board",
);
checkExpect(
  initialState("2 1"),
  State([[E, E]], Ongoing(P1)),
  "initialize a 2 by 1 board",
);
checkExpect(
  initialState("3 3"),
  State([[E, E, E], [E, E, E], [E, E, E]], Ongoing(P1)),
  "initialize a 3 by 3 board",
);
checkExpect(
  initialState("1 2"),
  State([[E], [E]], Ongoing(P1)),
  "initialize a 1 by 2 board",
);

// Test Cases for stringOfPlayer:
checkExpect(stringOfPlayer(P1), "Player 1", "represent player 1");
checkExpect(stringOfPlayer(P2), "Player 2", "represent player 2");

// Test Cases for stringOfCell:
checkExpect(stringOfCell(E), " ", "empty cell");
checkExpect(stringOfCell(R), "R", "red cell");
checkExpect(stringOfCell(B), "B", "blue cell");
checkExpect(stringOfCell(G), "G", "green/bogus cell");

// Test Cases/CheckErrors for stringOfColumn:
checkExpect(stringOfColumn(["E"]), "[E]", "1 element in the column");
checkExpect(
  stringOfColumn(["E", "E"]),
  "[E] [E]",
  "2 elements in the column",
);
checkExpect(
  stringOfColumn(["E", "E", "E"]),
  "[E] [E] [E]",
  "3 elements in the column",
);
checkError(() => stringOfColumn([]), "Board cannot have an empty column");

// Test Cases/CheckErrors for boardPrintHelper:
checkExpect(boardPrintHelper([[E]]), "[ ]\n", "printing a 1 by 1 board");
checkExpect(
  boardPrintHelper([[E], [E], [E]]),
  "[ ]\n[ ]\n[ ]\n",
  "printing a 3 by 1 board",
);
checkExpect(
  boardPrintHelper([[E, E, E], [E, E, E], [E, E, E]]),
  "[ ] [ ] [ ]\n[ ] [ ] [ ]\n[ ] [ ] [ ]\n",
  "printing a 3 by 1 board",
);

// Test Cases/CheckErrors for transpose:
checkExpect(
  transpose([[E, R], [R, B], [B, B]]),
  [[E, R, B], [R, B, B]],
  "3 by 2 board",
);
checkExpect(
  transpose([[E, E, E], [R, B, B]]),
  [[E, R], [E, B], [E, B]],
  "2 by 3 board",
);
checkExpect(transpose([[B]]), [[B]], "1 by 1 board");
checkExpect(transpose([[R, B]]), [[R], [B]], "1 by 2 board");

// Test Cases/CheckErrors for stringOfState:
checkExpect(
  stringOfState(State([[E, E], [E, E]], Draw)),
  "[ ] [ ]\n[ ] [ ]\n",
  "2 by 2 board and a draw",
);
checkExpect(
  stringOfState(State([[R, B, R], [B, B, B], [R, R, R]], Win(P2))),
  "[R] [B] [R]\n[B] [B] [R]\n[R] [B] [R]\n",
  "3 by 3 board and a win",
);

// Test Cases/CheckErrors for stringOfMove:
checkExpect(stringOfMove(Move(1)), "1", "move into column 1");
checkExpect(stringOfMove(Move(6)), "6", "move into column 6");
checkExpect(stringOfMove(Move(7)), "7", "move into column 7");

// Test Cases for legalMoves;
checkExpect(
  legalMoves(State([[R, B, R], [B, B, B], [R, R, R]], Win(P2))),
  [],
  "no legal moves available",
);
checkExpect(
  legalMoves(State([[E, B, R], [B, B, B], [R, R, R]], Ongoing(P2))),
  [Move(1)],
  "1 legal move available",
);
checkExpect(
  legalMoves(
    State([[E, B, R, R], [R, B, B, B], [E, E, R, B]], Ongoing(P1)),
  ),
  [Move(1), Move(3)],
  "2 legal moves available",
);

// Test Cases for gameStatus:
checkExpect(
  gameStatus(State([[E, E], [E, E]], Draw)),
  Draw,
  "draw status",
);
checkExpect(
  gameStatus(State([[E, R]], Win(P1))),
  Win(P1),
  "Player 1 wins",
);
checkExpect(
  gameStatus(State([[E, B], [E, E], [R, R]], Win(P2))),
  Win(P2),
  "Player 2 wins",
);

// Test Cases for moveOfString:
checkExpect(
  moveOfString("1", State([[E, R]], Ongoing(P1))),
  Move(1),
  "testing move(1)",
);
checkExpect(
  moveOfString("2", State([[E, R], [E, R]], Ongoing(P1))),
  Move(2),
  "testing move(2)",
);
checkError(
  () => moveOfString("3", State([[E, R], [E, R]], Ongoing(P1))),
  "Not a legal move; choose another move",
);
checkError(
  () => moveOfString("hi", State([[E, R]], Ongoing(P2))),
  "Not a legal move; choose another move",
);
checkError(
  () => moveOfString("2", State([[E, R], [E, E]], Win(P2))),
  "Not a legal move; choose another move",
);
checkError(
  () => moveOfString("1", State([[E, R]], Win(P1))),
  "Not a legal move; choose another move",
);
checkError(
  () => moveOfString("1", State([[E, R]], Draw)),
  "Not a legal move; choose another move",
);

// Test Cases for horzFlip:
checkExpect(
  horzFlip([[R, B], [B, R], [E, E]]),
  [[E, E], [B, R], [R, B]],
  "3 by 2 board",
);
checkExpect(
  horzFlip([[B, R, B], [E, E, R]]),
  [[E, E, R], [B, R, B]],
  "2 by 3 board",
);
checkExpect(
  horzFlip([[E, R, R, B, B], [E, E, R, B, R], [R, R, B, R, R]]),
  [[R, R, B, R, R], [E, E, R, B, R], [E, R, R, B, B]],
  "3 by 5 board",
);

// Test Cases for diagonalHelper:
checkExpect(
  diagonalHelper([[R, R, R, R], [R, R, R, R], [R, R, R, R]]),
  [[R, R, R, R, G, G], [G, R, R, R, R, G], [G, G, R, R, R, R]],
  "skewing a board full of R's",
);
checkExpect(
  diagonalHelper([[R, R, R, B], [R, R, B, R], [R, B, R, R]]),
  [[R, R, R, B, G, G], [G, R, R, B, R, G], [G, G, R, B, R, R]],
  "skewing a board with a 3-in-a-row blue diagonal",
);

// Test Cases for produceOutcome:
checkExpect(produceOutcome(P2, [R, R, R, B], []), Draw, "draw status");
checkExpect(
  produceOutcome(P2, [R, R, R, R], [Move(1), Move(2)]),
  Win(P2),
  "P2 wins",
);
checkExpect(
  produceOutcome(P1, [B, B, B, B], [Move(2), Move(3)]),
  Win(P1),
  "P1 wins",
);

// Test Cases for changeBoardHelper:
checkExpect(
  changeBoardHelper(
    [[E, R, R, R], [B, B, R, B], [R, R, B, R], [B, R, B, R]],
    P1,
    0,
  ),
  [[B, R, R, R], [B, B, R, B], [R, R, B, R], [B, R, B, R]],
  "P1 places a piece into column 1",
);
checkExpect(
  changeBoardHelper(
    [[E, E, R, R], [E, B, B, B], [R, B, B, R], [B, B, B, R]],
    P2,
    0,
  ),
  [[E, R, R, R], [E, B, B, B], [R, B, B, R], [B, B, B, R]],
  "P2 places a piece into column 1",
);
checkError(
  () =>
    changeBoardHelper(
      [[B, B, R, R], [R, B, B, B], [R, B, B, R], [B, B, B, R]],
      P2,
      1,
    ),
  "Column should have at least 1 empty cell when the player makes a move",
);
checkError(
  () =>
    changeBoardHelper(
      [[R, B, R, R], [R, B, B, B], [R, B, B, R], [B, B, B, R]],
      P2,
      1,
    ),
  "Column should have at least 1 empty cell when the player makes a move",
);

// Test Cases for checkFourInARow:
checkExpect(
  checkFourInARow(
    [[R, R, R, R], [E, E, B, R], [B, R, R, R], [E, B, R, B]],
    P2,
  ),
  Win(P2),
  "P2 wins, 4 in a row vertically",
);
checkExpect(
  checkFourInARow(
    [[B, R, R, B], [B, B, B, R], [B, B, R, R], [B, B, R, B]],
    P1,
  ),
  Win(P1),
  "P1 wins, 4 in a row horizontally",
);
checkExpect(
  checkFourInARow(
    [[B, R, R, B], [B, B, B, R], [B, B, R, R], [B, R, B, B]],
    P1,
  ),
  Win(P1),
  "P1 wins, 4 in a row diagonally",
);
checkExpect(
  checkFourInARow(
    [[R, R, R, B], [B, R, B, R], [B, B, R, R], [B, R, B, R]],
    P2,
  ),
  Win(P2),
  "P2 wins, 4 in a row diagonally",
);

// Test Cases/checkErrors for nextState:
checkExpect(
  nextState(
    State(
      [[E, E, E, E], [E, E, E, E], [E, E, E, E], [E, E, E, E]],
      Ongoing(P2),
    ),
    Move(1),
  ),
  State(
    [[E, E, E, R], [E, E, E, E], [E, E, E, E], [E, E, E, E]],
    Ongoing(P1),
  ),
  "game is ongoing",
);
checkExpect(
  nextState(
    State(
      [[B, R, B, R], [E, R, R, B], [E, B, R, R], [B, B, R, R]],
      Ongoing(P1),
    ),
    Move(2),
  ),
  State(
    [[B, R, B, R], [B, R, R, B], [E, B, R, R], [B, B, R, R]],
    Ongoing(P2),
  ),
  "P1 wins",
);
checkExpect(
  nextState(
    State(
      [[B, B, B, R], [E, R, R, R], [E, B, R, R], [B, B, R, R]],
      Ongoing(P2),
    ),
    Move(2),
  ),
  State(
    [[B, B, B, R], [R, R, R, R], [E, B, R, R], [B, B, R, R]],
    Win(P2),
  ),
  "P2 wins",
);
checkExpect(
  nextState(
    State(
      [[R, R, B, R], [E, E, R, R], [E, R, R, R], [R, B, R, R]],
      Ongoing(P2),
    ),
    Move(3),
  ),
  State(
    [[R, R, B, R], [E, E, R, R], [R, R, R, R], [R, B, R, R]],
    Win(P2),
  ),
  "P2 wins",
);
checkExpect(
  nextState(
    State(
      [[E, B, B, B], [E, R, R, R], [B, B, R, B], [R, B, R, R]],
      Ongoing(P1),
    ),
    Move(1),
  ),
  State(
    [[B, B, B, B], [E, R, R, R], [B, B, R, B], [R, B, R, R]],
    Win(P1),
  ),
  "P1 wins",
);

// Test Cases for pointsForXInAColumn:
checkExpect(
  pointsForXInAColumn([E, B, B, B, B]),
  9.99 *. 10.0 ** 80.0,
  "4 in a row for Blue",
);
checkExpect(
  pointsForXInAColumn([B, R, R, R, R]),
  (-9.99) *. 10.0 ** 80.0,
  "4 in a row for Red",
);
checkExpect(
  pointsForXInAColumn([R, R, R, B, B]),
  100000000000000.1,
  "blocking red from getting 4 in a row",
);
checkExpect(pointsForXInAColumn([E, R, R, B, B]), 0., "2 in a row for R");

// Test Cases for totalPointsForState:
checkExpect(totalPointsForState([[E, R, R, B, B]]), 0., "evaluating a row");
checkExpect(
  totalPointsForState([[E, E, E, E, E]]),
  0.0,
  "evaluating a row",
);
checkExpect(
  totalPointsForState([[R, R, R, B, B]]),
  99999999999999.8,
  "evaluating a row",
);

// Test Cases for estimateValue:
checkExpect(
  estimateValue(State([[R, R, R, B, B]], Ongoing(P2))),
  99999999999999.8,
  "evaluating a position",
);
checkExpect(
  estimateValue(State([[R, B, B, B, B]], Ongoing(P1))),
  9.99000000000000111e+80,
  "evaluating a position",
);
checkExpect(
  estimateValue(State([[R, R], [B, B]], Ongoing(P1))),
  0.4,
  "evaluating a position",
);
checkExpect(
  estimateValue(
    State(
      [
        [R, R, R, B, B],
        [E, B, R, R, B],
        [E, E, E, E, E],
        [R, B, B, B, R],
      ],
      Ongoing(P2),
    ),
  ),
  -900000000008899.4,
  "evaluating a position",
);