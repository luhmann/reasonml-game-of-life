[@bs.module] external logUpdate: string => unit = "log-update";
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

let createCondition = () => Random.int(10) < 4 ? Alive : Dead;

let initialBoard = Array.make_matrix(30, 80, 0);

let seed = board =>
  Array.map(row => Array.map(_col => createCondition(), row), board);

let board = ref(seed(initialBoard));

let printCondition = (cond: condition) =>
  switch (cond) {
  | Alive => {js|◽|js}
  | Dead => {js|◾|js}
  | OutOfBounds => ""
  };

let printRow = row =>
  Js.Array.joinWith(" ", Array.map(item => printCondition(item), row));

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

        Js.Array.push(value, items) |> ignore;
      };
    };
  };

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

let printBoard = board =>
  logUpdate(
    Js.Array.joinWith("\n", Array.map(row => printRow(row), board)),
  );

printBoard(board^);
Js.Global.setInterval(
  () => {
    board := tick(board);
    printBoard(board^);
  },
  250,
);
