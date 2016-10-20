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
  ( Board, make, makeWithInitial, get, set, getRow, setRow
  )

{-| Two-dimensional game board with integers as elements.

@docs Board
@docs make
@docs makeWithInitial
@docs get
@docs set
@docs getRow
@docs setRow

-}

import Array exposing (Array)

{-|-}
type alias Board a =
  { rows : Int
  , cols : Int
  , defaultValue: a
  , array: Array (Array a)
  }
      
makeRow : Int -> a -> (Array a)
makeRow cols defaultValue =
  Array.repeat cols defaultValue

{-| Create a new Board of the given size, initialized with the defaultValue value.

    make rows cols defaultValue
-}
make : Int -> Int -> a -> Board a
make rows cols defaultValue =
  makeWithInitial rows cols defaultValue defaultValue

{-| Create a new Board of the given size, initialized with a different value.

    makeWithInitial rows cols defaultValue initial
-}
makeWithInitial : Int -> Int -> a -> a -> Board a
makeWithInitial rows cols defaultValue initial =
  Board
    rows
    cols
    defaultValue
    (Array.repeat rows (makeRow cols initial))
  

check : Board a -> Int -> Int -> Bool
check board row col =
  row>=0 && row<board.rows && col>=0 && col<board.cols

{-| Get a single element. Returns 0 if row or col is out of range.

    get row col board
-}
get : Int -> Int -> Board a -> a
get row col board =
  case Array.get row board.array of
      Nothing -> board.defaultValue
      Just r ->
        case Array.get col r of
            Nothing -> board.defaultValue
            Just res -> res

{-| Set a single element. Does nothing if row or col is out of range.

    set row col val board
-}
set : Int -> Int -> a -> Board a -> Board a
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
getRow : Int -> Board a -> Array a
getRow row board =
  case Array.get row board.array of
      Nothing -> makeRow board.cols board.defaultValue
      Just row -> row

{-| Set row in board to rowArray. Do nothing if row is out of range.

    setRow row rowArray board
-}
setRow : Int -> Array a -> Board a -> Board a
setRow row rowArray board =
  { board | array = Array.set row rowArray board.array }
