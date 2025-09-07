module Sudoku where

-- a cell can be either filled with a given number or empty with the list of potential values
data Cell = Empty [Int] | Filled Int deriving Show

-- a board is a list of cells
newtype Board = Board [Cell]

instance Show Board where
    show :: Board -> String
    show (Board cells) = concat $ zipWith (++) (map show cells) (cycle separators) where
        separators = replicate 8 " " ++ ["\n"]

-- initial board consists of 9 × 9 = 81 open cells, each with no values in it
emptyBoard :: Board
emptyBoard = Board (replicate 81 (Empty [1..9]))

updateBoard :: Int -> (Int, Int) -> Board -> Board
updateBoard value (r, c) (Board cells) = Board (xs ++ [Filled value] ++ ys) where
    index = 9 * r + c
    (xs, y:ys) = splitAt index cells
