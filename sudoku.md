# A Sudoku solver in Haskell

First try to implement one on my own, then check books/articles/posts...

## What is Sudoku?

## Starting point

To play Sudoku, we need a 9 × 9 grid, whose cells can be either filled with numbers (from 1 to 9), or be empty. This suggests that cells are nothing more than `Maybe Int`s, and a grid is a list of cells with length of 81.

```haskell
module Sudoku where

-- a cell can be either filled with a given number or be empty
type Cell = Maybe Int

-- a board is a list of cells
type Board = [Cell]

-- initial board consists of 9 × 9 = 81 open cells, each with no values in it
emptyBoard :: Board
emptyBoard = replicate 81 Nothing
```

As a starting point this seems OK, however there are a couple of things that could be improved:

* we use `Int` type in `Cell`s, however the possible value range is just 1 to 9,
* we define the board as an arbitrary-length list, however we know the exact length, which is 81,
* the board is naturally a 2-dimensional object with row and column coordinates, but we created it as a 1-dimensional list, and conversion between (row, column) coordinates and list indices is not ideal, and might be error-prone.

Another thing that looks bad is how the board is shown in GHCi:

```shell
ghci> emptyBoard
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
```

We really have to show the board as a 2-dimensional entity, and we will achieve this by redefining `Show` for `Board`.

### A better show

In order to do this, we modify `Board`, and use `newtype` to be able to define the `Show` instance of `Board` as we wish.

```haskell
newtype Board = Board [Cell]
```

How should our board be printed? First of all, every ninth entry, we would like to have a newline, and also the cells should be separated from each other. Moreover, our cells should be displayed using their own `show`.

```haskell
instance Show Board where
    show :: Board -> String
    show (Board cells) = concat $ zipWith (++) (map show cells) (cycle separators) where
        separators = replicate 8 " " ++ ["\n"]
```

With this, we get the following display, that is definitely better than it was previously, and seems good enough for now.

```shell
ghci> emptyBoard
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

```
