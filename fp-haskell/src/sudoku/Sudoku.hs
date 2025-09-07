module Sudoku where

import Data.List (delete)

-- a cell can be either filled with a given number or empty with the list of potential values
data Cell = Empty [Int] | Filled Int

wsCenter :: Int -> String -> String
wsCenter n s
    | len >= n  = s
    | otherwise = (replicate l ' ') ++ s ++ (replicate r ' ')
    where
        len = length s
        d   = n - len
        r   = d `quot` 2
        l   = d - r

instance Show Cell where
    show (Empty ps) = wsCenter 11 $ "[" ++ concatMap show ps ++ "]"
    show (Filled val) = wsCenter 11 $ show val
    

-- a board is a list of cells
newtype Board = Board [Cell]

instance Show Board where
    show :: Board -> String
    show (Board cells) = concat $ zipWith (++) (map show cells) (cycle separators) where
        separators = replicate 8 " " ++ ["\n"]

-- initial board consists of 9 × 9 = 81 open cells, each with no values in it
emptyBoard :: Board
emptyBoard = Board (replicate 81 (Empty [1..9]))

sameCell :: (Int, Int) -> (Int, Int) -> Bool
sameCell (r, c) (r', c') = (r' == r) && (c' == c)

sameUnit :: (Int, Int) -> (Int, Int) -> Bool
sameUnit (r, c) (r', c') = (r' == r) || (c' == c) || ((div r' 3 == div r 3) && (div c' 3 == div c 3))

updateBoard :: Int -> (Int, Int) -> Board -> Board
updateBoard value (r, c) (Board cells) = Board $ map (updateCell value (r, c)) (zip [0..] cells) where
    updateCell :: Int -> (Int, Int) -> (Int, Cell) -> Cell
    updateCell value (r, c) (index, cell)
        | sameCell (r, c) (r', c') = Filled value
        | sameUnit (r, c) (r', c') = case cell of
            (Filled value') -> Filled value'
            (Empty ps) -> Empty (delete value ps)
        | otherwise = cell
        where
            r' = div index 9
            c' = mod index 9
