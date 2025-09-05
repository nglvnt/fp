module Sudoku where

-- a cell can be either filled with a given number or be empty
type Cell = Maybe Int

-- a board is a list of cells
type Board = [Cell]

-- initial board consists of 9 × 9 = 81 open cells, each with no values in it
emptyBoard :: Board
emptyBoard = replicate 81 Nothing
