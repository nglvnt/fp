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

### Adding a value to a cell

The empty board is not too interesting, we have to start filling it with numbers. The only complication is the one we have already observed: it is more natural and easy to use (row, column) coordinates when referring to a cell, than using a flat index. We assume that that row and column coordinates start at 0, just like list indices, this makes the coordinate-to-index conversion a bit easier (otherwise we would have to subtract 1 from the coordinates).Also, the fact that `splitAt` works is kind of magical, I had to check for both edge cases, the first and last element of the board, that `updateBoard` really does what it should.

```haskell
updateBoard :: Int -> (Int, Int) -> Board -> Board
updateBoard value (r, c) (Board cells) = Board (xs ++ [Just value] ++ ys) where
    index = 9 * r + c
    (xs, y:ys) = splitAt index cells
```

What improvement opportunities can we note down?

* the coordinate-index conversion that has been already commented,
* row and column coordinates defined as `Int`s, though they could take just a restricted set of values,
* the `(xs, y:ys) = splitAt index cells` part is hard to follow,
* `Just n` has length 6, one less than `Nothing`, causing the discrepancy in the displayed board.

```shell
ghci> updateBoard 1 (0, 0) emptyBoard
Just 1 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

ghci> updateBoard 1 (8, 8) emptyBoard
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Just 1

```

### Calculating the next move

Now that we have a way to fill the cells of a board, we can start working on how to determine what should be the next value to be added to the board. The most basic thing we can do is to go through the cells, and for each cell we check the existing values in other cells in the same row, column, or 3 × 3 subsquare, and determine what are the remaining possible values for the cell. If there is only one possibility, then we fill the cell with that value, and iterate this process.

However, this constant recalculation feels wasteful, maybe we can utilise the cells to keep track their possible vales while filling up the board. For this, let's look at the definition of a cell:

```haskell
type Cell = Maybe Int
```

Equivalently and explicitly:

```haskell
type Cell = Nothing | Just Int
```

A cell can be either filled with a given value, the `Just Int` part, or can be empty (the `Nothing` part), and in this case, the information content of the cell is its collection of potential values. This suggests the following modification:

```haskell
data Cell = Empty [Int] | Filled Int
```

After this change, we immediately get type errors in the emptyBoard and updateBoard definitions and let's address them. And after that we get that there is no `Show` instance defined for our new `Cell`, let's do that by simply deriving `Show`.

```haskell
data Cell = Empty [Int] | Filled Int deriving Show

...

emptyBoard :: Board
emptyBoard = Board (replicate 81 (Empty [1..9]))

updateBoard :: Int -> (Int, Int) -> Board -> Board
updateBoard value (r, c) (Board cells) = Board (xs ++ [Filled value] ++ ys) where
    index = 9 * r + c
    (xs, y:ys) = splitAt index cells
```

Reload the module, and check `emptyBoard` and `updateBoard`.

```shell
ghci> emptyBoard
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]

ghci> updateBoard 1 (0, 0) emptyBoard
Filled 1 Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9] Empty [1,2,3,4,5,6,7,8,9]
```

We have two problems now: the display looks bad, and `updateBoard` just fills up the value in a cell, does not modify the potential values in other cells.

### Looking good

We are going to address the aesthetic issue. The display we see in the terminal is the result of two steps: first we display the cells, then we use them for displaying the board. When we first modified the display, we had a problem with the look of the board, but now the issue is rather with the cells. The cause is that we were lazy, and just derived the `Show` instance for our `Cell` datatype, giving the unsatisfactory look.

How could a cell be displayed? The displayed data constructors `Filled` and `Empty` seem superfluous, maybe we can indicate that a cell is filled with a simple number, and the empty cells with the list of potential values.

```haskell
data Cell = Empty [Int] | Filled Int

instance Show Cell where
    show (Empty ps) = show ps
    show (Filled val) = show val
```

```shell
ghci> emptyBoard
[1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9]
[1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9]
[1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9]
[1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9]
[1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9]
[1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9]
[1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9]
[1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9]
[1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9]

```

We do not even need the commas.

```haskell
instance Show Cell where
    show (Empty ps) = "[" ++ concatMap show ps ++ "]"
    show (Filled val) = show val
```

Now it looks more compact.

```shell
ghci> emptyBoard
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]

```

But we still have the string length discrepancy.

```shell
ghci> updateBoard 1 (0, 0) emptyBoard
1 [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]

```

We fix this by padding strings with whitespace to a given length. The longest possible string to be displayed is the empty cell with 1 to 9 as potential values and with the square brackets, giving a total length of 11, so we will pad every string to this length.

```haskell
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
```

```shell
ghci> updateBoard 1 (0, 0) emptyBoard
     1      [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
[123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]

```

Much better!(?)

### Updating the potential values

Let's move on to `updateBoard`! What we need is that when we fill a cell with a value, we also remove that value from the list of potential values from all the other cells in the same row/column/3×3 subsquare.

How de we find the other cells in the same row/column/3×3 subsquare? In `(row, column)` coordinate form, it is easy to tell if two cells are in the same row or column: same value in the first or second coordinate. Regarding the 3×3 subsquare, we might integer-divide the coordinates by 3, and check if we get the same pair as a result.

```haskell
sameCell :: (Int, Int) -> (Int, Int) -> Bool
sameCell (r, c) (r', c') = (r' == r) && (c' == c)

sameUnit :: (Int, Int) -> (Int, Int) -> Bool
sameUnit (r, c) (r', c') = (r' == r) || (c' == c) || ((div r' 3 == div r 3) && (div c' 3 == div c 3))
```

Modified `updateBoard` will do the following: it will go though all the cells of the board, check if a cell is

* the one that we target with our update, then we make it filled with the value,
* or in the same row/column/3×3 subsquare of the target cell, then we update the list of potential values if it is an empty cell or leave as it is if filled,
* or leave as it is.

The lack of coordinates in a cell means that we have to keep track the index (`zip [0..] cells`) and then convert the index to coordinate (`r' = div index 9` and `c' = mode index 9`), not an ideal solution.

We also do not check if the target cell is already a filled cell, just overwrite it in any case with a filled cell. What should we do in this case?

```haskell
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
```

Let's see how it works.

```shell
ghci> updateBoard 1 (0, 0) emptyBoard
     1       [23456789]  [23456789]  [23456789]  [23456789]  [23456789]  [23456789]  [23456789]  [23456789]
 [23456789]  [23456789]  [23456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
 [23456789]  [23456789]  [23456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
 [23456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
 [23456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
 [23456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
 [23456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
 [23456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]
 [23456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789] [123456789]

```

## Solving the first Sudoku puzzle

So far we concentrated on managing the board, now we can turn our attention to actually solving a Sudoku puzzle.

### An easy example

To solve a Sudoku, first we need an actual Sudoku puzzle. The initial state of a Sudoku board can be described by value-coordinate pairs, giving us the initial values at the given coordinates. We can put these value-coordinate pairs into a list, and use `updateBoard` to build up the initial Sudoku board.

The first example will be an easy one, with 38 numbers already given, the board is almost 50% filled.

```haskell
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
```

Given this input list, we can fold through this list, updating the board with the value in the coordinate, and then passing the result board to the next `updateBoard` iteration.

```haskell
firstExampleStartBoard = foldl (\b (v, c) -> updateBoard v c b) emptyBoard firstExampleInput
```

The initial state is the following.

```shell
ghci> firstExampleStartBoard
   [248]       [478]         1           9          [2]          6         [238]       [248]         5     
   [2468]      [4568]       [6]         [45]         7         [2345]      [238]       [2489]        1     
     3          [45]         9           8         [125]       [1245]        7          [24]         6     
    [1]        [139]         2           6           8         [159]         4           7          [9]    
     7         [169]         4           2         [159]       [159]       [568]         3          [89]   
     5          [69]         8           7           3          [49]         1          [69]         2     
    [4]          2           7          [5]          6           8           9           1           3     
     9         [168]         5           3           4         [127]       [268]       [268]        [78]   
   [1468]     [13468]       [36]        [1]        [129]       [1279]      [268]         5         [478]
```

### Filling single-possibility cells

Checking the empty cells with the list of possibilities, we see that there are cells with single possible value, hence a first, natural solution strategy could be that we fill these cells. We need a function to find these cells, and extract the value-coordinate pairs from them, and then we just simply fold through them to update the board.

To get the coordinates, we enumerate the cells, and use integer division and remainder to convert the cell index to the coordinate, not an ideal solution, but works for now.

```haskell
findSinglePossibilities :: Board -> [(Int, (Int, Int))]
findSinglePossibilities (Board cells) = do
    (index, cell) <- zip [0..] cells
    case cell of
        Filled _ -> []
        Empty [value] -> [(value, (div index 9, mod index 9))]
        Empty _ -> []
```

```shell
ghci> findSinglePossibilities firstExampleStartBoard
[(2,(0,4)),(6,(1,2)),(1,(3,0)),(9,(3,8)),(4,(6,0)),(5,(6,3)),(1,(8,3))]
```

As it will be used many times, we define the following generalization of `updateBoard` to a list of value-coordinate pairs.

```haskell
updateBoardMany :: [(Int, (Int, Int))] -> Board -> Board
updateBoardMany xs board = foldl (\b (v, c) -> updateBoard v c b) board xs
```

With this, a step towards solution can be written as

```shell
ghci> updateBoardMany (findSinglePossibilities firstExampleStartBoard) firstExampleStartBoard
    [8]        [478]         1           9           2           6          [38]        [48]         5     
    [28]       [458]         6          [4]          7         [345]       [238]       [2489]        1     
     3          [45]         9           8          [15]       [145]         7          [24]         6     
     1          [3]          2           6           8          [5]          4           7           9     
     7          [69]         4           2         [159]       [159]       [568]         3          [8]    
     5          [69]         8           7           3          [49]         1          [6]          2     
     4           2           7           5           6           8           9           1           3     
     9         [168]         5           3           4          [27]       [268]       [268]        [78]   
    [68]       [368]        [3]          1          [9]        [279]       [268]         5         [478]
```
