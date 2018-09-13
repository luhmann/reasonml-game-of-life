Random.init(int_of_float(Js.Date.now()));

/**
 * Compute a list of integers starting with `start`,
 * up to and excluding `end_`.
 */
let rec range = (start: int, end_: int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  };

type condition =
  | Alive
  | Dead
  | OutOfBounds;

type board = ref(array(array(condition)));

type coordinates = (int, int);

let createCondition = () => Random.int(10) < 5 ? Alive : Dead;

let initialBoard = Array.make_matrix(20, 50, 0);

let seed = board =>
  Array.map(row => Array.map(_col => createCondition(), row), board);

let board = ref(seed(initialBoard));

let printCondition = (cond: condition) =>
  switch (cond) {
  | Alive => print_string({js|◽ |js})
  | Dead => print_string({js|◾ |js})
  | OutOfBounds => ()
  };

/* let printCondition = (cond: condition) =>
   switch (cond) {
   | Alive => print_string("1|")
   | Dead => print_string("0|")
   }; */

let printRow = row: unit => {
  Array.map(item => printCondition(item), row) |> ignore;
  print_string("\n");
};

let getAliveNeighbours = (coordinates: coordinates): int => {
  let (x, y) = coordinates;
  let items = [||];
  for (colOffset in (-1) to 1) {
    for (rowOffset in (-1) to 1) {
      let targetCol = x + colOffset;
      let targetRow = y + rowOffset;

      if ((targetCol, targetRow) != coordinates) {
        let value =
          switch (board^[targetRow][targetCol]) {
          | condition => condition
          | exception (Invalid_argument(_err)) => OutOfBounds
          };
        /*
         Js.log3(
           coordinates,
           (targetCol, targetRow),
           switch (value) {
           | Alive => "1"
           | Dead => "0"
           | OutOfBounds => "X"
           },
         ); */

        Js.Array.push(value, items) |> ignore;
      };
    };
  };
  /* print_endline("---"); */
  let sum =
    Array.fold_left(
      (acc, item) =>
        switch (item) {
        | Alive => acc + 1
        | Dead => acc
        | OutOfBounds => acc
        },
      0,
      items,
    );
  /* Js.log3(
       coordinates,
       Array.map(item => item == Alive ? "1" : "0", items),
       sum,
     ); */
  sum;
};

let isDeadOrAlive = (condition, aliveNeighbours) =>
  switch (condition, aliveNeighbours) {
  | (Alive, 2) => Alive
  | (Alive, 3) => Alive
  | (Dead, 3) => Alive
  | _ => Dead
  };

let tick = (board: board) =>
  Array.mapi(
    (rowIndex, row) =>
      Array.mapi(
        (colIndex, cell) =>
          isDeadOrAlive(cell, getAliveNeighbours((colIndex, rowIndex))),
        row,
      ),
    board^,
  );

let printBoard = board => {
  Js.log("\x1Bc");
  /* Js.log("--------"); */
  Array.map(row => printRow(row), board) |> ignore;
};

/* printBoard(board^);
   printBoard(tick(board)); */

printBoard(board^);
Js.Global.setInterval(
  () => {
    board := tick(board);
    printBoard(board^);
  },
  1000,
);
