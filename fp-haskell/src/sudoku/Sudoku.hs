module Sudoku where

-- a cell can be either filled with a given number or be empty
type Cell = Maybe Int

-- a board is a list of cells
newtype Board = Board [Cell]

instance Show Board where
    show :: Board -> String
    show (Board cells) = concat $ zipWith (++) (map show cells) (cycle separators) where
        separators = replicate 8 " " ++ ["\n"]

-- initial board consists of 9 × 9 = 81 open cells, each with no values in it
emptyBoard :: Board
emptyBoard = Board (replicate 81 Nothing)
