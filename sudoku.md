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
