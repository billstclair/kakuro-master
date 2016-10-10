----------------------------------------------------------------------
--
-- Board.elm
-- Rectangular game board with integer elements
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Board exposing
  ( Board, make, get, set, getRow, setRow
  )

{-| Two-dimensional game board with integers as elements.

@docs Board
@docs make
@docs get
@docs set
@docs getRow
@docs setRow

-}

import Array exposing (Array)

{-|-}
type alias Board =
  { rows : Int
  , cols : Int
  , array: Array (Array Int)
  }
      
makeRow : Int -> (Array Int)
makeRow cols =
  Array.repeat cols 0

{-| Create a new Board of the given size.

    make rows cols
-}
make : Int -> Int -> Board
make rows cols =
  Board
    rows
    cols
    (Array.repeat rows (makeRow cols))

check : Board -> Int -> Int -> Bool
check board row col =
  row>=0 && row<board.rows && col>=0 && col<board.cols

{-| Get a single element. Returns 0 if row or col is out of range.

    get row col board
-}
get : Int -> Int -> Board -> Int
get row col board =
  case Array.get row board.array of
      Nothing -> 0
      Just r ->
        case Array.get col r of
            Nothing -> 0
            Just res -> res

{-| Set a single element. Does nothing if row or col is out of range.

    set row col val board
-}
set : Int -> Int -> Int -> Board -> Board
set row col val board =
  if not (check board row col) then
    board
  else
    case Array.get row board.array of
       Nothing -> board
       Just r -> { board |
                     array = Array.set
                               row
                               (Array.set col val r)
                               board.array
                 }

{-| Return the Array for the given row, or an all-zero array, if out of range.

    getRow row board
-}
getRow : Int -> Board -> Array Int
getRow row board =
  case Array.get row board.array of
      Nothing -> makeRow board.cols
      Just row -> row

{-| Set row in board to rowArray. Do nothing if row is out of range.

    setRow row rowArray board
-}
setRow : Int -> Array Int -> Board -> Board
setRow row rowArray board =
  { board | array = Array.set row rowArray board.array }
  
