// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Js_exn = require("bs-platform/lib/js/js_exn.js");
var Random = require("bs-platform/lib/js/random.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

Random.init(Date.now() | 0);

function range(start, end_) {
  if (start >= end_) {
    return /* [] */0;
  } else {
    return /* :: */[
            start,
            range(start + 1 | 0, end_)
          ];
  }
}

function createCondition() {
  var match = Random.$$int(10) < 5;
  if (match) {
    return /* Alive */0;
  } else {
    return /* Dead */1;
  }
}

var initialBoard = $$Array.make_matrix(20, 50, 0);

function seed(board) {
  return $$Array.map((function (row) {
                return $$Array.map((function () {
                              return createCondition(/* () */0);
                            }), row);
              }), board);
}

var board = /* record */[/* contents */seed(initialBoard)];

function printCondition(cond) {
  switch (cond) {
    case 0 : 
        return Pervasives.print_string("◽ ");
    case 1 : 
        return Pervasives.print_string("◾ ");
    case 2 : 
        return /* () */0;
    
  }
}

function printRow(row) {
  $$Array.map(printCondition, row);
  return Pervasives.print_string("\n");
}

function getAliveNeighbours(coordinates) {
  var y = coordinates[1];
  var x = coordinates[0];
  var items = /* array */[];
  for(var colOffset = -1; colOffset <= 1; ++colOffset){
    for(var rowOffset = -1; rowOffset <= 1; ++rowOffset){
      var targetCol = x + colOffset | 0;
      var targetRow = y + rowOffset | 0;
      if (Caml_obj.caml_notequal(/* tuple */[
              targetCol,
              targetRow
            ], coordinates)) {
        var value;
        try {
          value = Caml_array.caml_array_get(Caml_array.caml_array_get(board[0], targetRow), targetCol);
        }
        catch (raw_exn){
          var exn = Js_exn.internalToOCamlException(raw_exn);
          if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
            value = /* OutOfBounds */2;
          } else {
            throw exn;
          }
        }
        items.push(value);
      }
      
    }
  }
  return $$Array.fold_left((function (acc, item) {
                if (item !== 0) {
                  return acc;
                } else {
                  return acc + 1 | 0;
                }
              }), 0, items);
}

function isDeadOrAlive(condition, aliveNeighbours) {
  switch (condition) {
    case 0 : 
        if (aliveNeighbours === 3 || aliveNeighbours === 2) {
          return /* Alive */0;
        } else {
          return /* Dead */1;
        }
    case 1 : 
        if (aliveNeighbours !== 3) {
          return /* Dead */1;
        } else {
          return /* Alive */0;
        }
    case 2 : 
        return /* Dead */1;
    
  }
}

function tick(board) {
  return $$Array.mapi((function (rowIndex, row) {
                return $$Array.mapi((function (colIndex, cell) {
                              return isDeadOrAlive(cell, getAliveNeighbours(/* tuple */[
                                              colIndex,
                                              rowIndex
                                            ]));
                            }), row);
              }), board[0]);
}

function printBoard(board) {
  console.log("\x1bc");
  $$Array.map(printRow, board);
  return /* () */0;
}

printBoard(board[0]);

setInterval((function () {
        board[0] = tick(board);
        return printBoard(board[0]);
      }), 1000);

exports.range = range;
exports.createCondition = createCondition;
exports.initialBoard = initialBoard;
exports.seed = seed;
exports.board = board;
exports.printCondition = printCondition;
exports.printRow = printRow;
exports.getAliveNeighbours = getAliveNeighbours;
exports.isDeadOrAlive = isDeadOrAlive;
exports.tick = tick;
exports.printBoard = printBoard;
/*  Not a pure module */
