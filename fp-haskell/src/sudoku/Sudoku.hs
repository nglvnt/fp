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

updateBoardMany :: [(Int, (Int, Int))] -> Board -> Board
updateBoardMany xs board = foldl (\b (v, c) -> updateBoard v c b) board xs

findSinglePossibilities :: Board -> [(Int, (Int, Int))]
findSinglePossibilities (Board cells) = do
    (index, cell) <- zip [0..] cells
    case cell of
        Filled _ -> []
        Empty [value] -> [(value, (div index 9, mod index 9))]
        Empty _ -> []

solveSudoku :: Board -> Board
solveSudoku board = case findSinglePossibilities board of
    [] -> board
    xs -> solveSudoku $ updateBoardMany xs board

-- examples

firstExampleInput = [
    (1, (0, 2)), (9, (0, 3)), (6, (0, 5)), (5, (0, 8)),
    (7, (1, 4)), (1, (1, 8)),
    (3, (2, 0)), (9, (2, 2)), (8, (2, 3)), (7, (2, 6)), (6, (2, 8)),
    (2, (3, 2)), (6, (3, 3)), (8, (3, 4)), (4, (3, 6)), (7, (3, 7)),
    (7, (4, 0)), (4, (4, 2)), (2, (4, 3)), (3, (4, 7)),
    (5, (5, 0)), (8, (5, 2)), (7, (5, 3)), (3, (5, 4)), (1, (5, 6)), (2, (5, 8)),
    (2, (6, 1)), (7, (6, 2)), (6, (6, 4)), (8, (6, 5)), (9, (6, 6)), (1, (6, 7)), (3, (6, 8)),
    (9, (7, 0)), (5, (7, 2)), (3, (7, 3)), (4, (7, 4)),
    (5, (8, 7))
    ] :: [(Int, (Int, Int))]

firstExampleStartBoard = foldl (\b (v, c) -> updateBoard v c b) emptyBoard firstExampleInput

secondExampleInput = [
    (7, (0, 2)), (3, (0, 4)), (4, (0, 6)), (6, (0, 8)),
    (9, (1, 0)), (3, (1, 1)), (8, (1, 8)),
    (8, (2, 0)), (2, (2, 1)), (1, (2, 3)), (9, (2, 4)), (7, (2, 6)),
    (1, (3, 0)), (4, (3, 1)), (8, (3, 5)),
    (2, (4, 4)), (8, (4, 7)), (5, (4, 8)),
    (8, (5, 2)), (6, (5, 4)),
    (6, (6, 1)), (9, (6, 3)), (8, (6, 4)), (5, (6, 6)),
    (3, (7, 0)), (1, (7, 2)), (7, (7, 5)), (6, (7, 6)),
    (9, (8, 2)), (1, (8, 4))
    ] :: [(Int, (Int, Int))]

secondExampleStartBoard = foldl (\b (v, c) -> updateBoard v c b) emptyBoard secondExampleInput
