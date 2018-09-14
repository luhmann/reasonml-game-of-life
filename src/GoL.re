Random.init(int_of_float(Js.Date.now()));

type condition =
  | Alive
  | Dead
  | OutOfBounds;

type board = ref(array(array(condition)));

type coordinates = (int, int);

module Output = {
  [@bs.module] external logUpdate: string => unit = "log-update";

  let mapCondition = cond =>
    switch (cond) {
    | Alive => {js|◽|js}
    | Dead => {js|◾|js}
    | OutOfBounds => ""
    };

  let mapRow = row: string =>
    row->Array.map(mapCondition, _)->Js.Array.joinWith(" ", _);

  let printBoard = board =>
    board->Array.map(mapRow, _)->Js.Array.joinWith("\n", _)->logUpdate;
};

let createCondition = () => Random.int(10) < 4 ? Alive : Dead;

/* NOTE: use array for speed of value access */
let initialBoard = Array.make_matrix(30, 80, 0);

let seed = board =>
  Array.map(row => Array.map(_col => createCondition(), row), board);

let board = ref(seed(initialBoard));

let neighbourOffsets = [
  ((-1), (-1)),
  ((-1), 0),
  ((-1), 1),
  (0, (-1)),
  (0, 1),
  (1, (-1)),
  (1, 0),
  (1, 1),
];

let getAliveNeighbours = (board, coordinates: coordinates): int => {
  let (x, y) = coordinates;
  List.map(
    ((rowOffset, colOffset)) =>
      switch (board^[y + rowOffset][x + colOffset]) {
      | condition => condition
      | exception (Invalid_argument(_err)) => OutOfBounds
      },
    neighbourOffsets,
  )
  ->List.fold_left(
      (acc, item) =>
        switch (item) {
        | Alive => acc + 1
        | Dead => acc
        | OutOfBounds => acc
        },
      0,
      _,
    );
};

let isDeadOrAlive = (condition, numAliveNeighbours) =>
  switch (condition, numAliveNeighbours) {
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
          isDeadOrAlive(
            cell,
            getAliveNeighbours(board, (colIndex, rowIndex)),
          ),
        row,
      ),
    board^,
  );

Output.printBoard(board^);
Js.Global.setInterval(
  () => {
    board := tick(board);
    Output.printBoard(board^);
  },
  250,
);
