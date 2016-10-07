module Board exposing
  ( Board, make, get, set, getRow
  )

import Array exposing (Array)

type alias Board =
  { rows : Int
  , cols : Int
  , array: Array (Array Int)
  }
      
make : Int -> Int -> Board
make rows cols =
  Board
    rows
    cols
    (Array.repeat rows (Array.repeat cols 0))

check : Board -> Int -> Int -> Bool
check board row col =
  row>=0 && row<board.rows && col>=0 && col<board.cols

get : Board -> Int -> Int -> Maybe Int
get board row col =
  case Array.get row board.array of
      Nothing -> Nothing
      Just r -> Array.get col r

setElement : (Array a) -> Int -> a -> (Array a)
setElement array idx val =
  Array.append
    (Array.slice 0 idx array)
    (Array.append
       (Array.repeat 1 val)
       (Array.slice (idx+1) (Array.length array) array))

set : Board -> Int -> Int -> Int -> Maybe Board
set board row col val =
  if not (check board row col) then
    Nothing
  else
    case Array.get row board.array of
       Nothing -> Nothing
       Just r -> Just (Board
                         board.rows
                         board.cols
                         (setElement
                            board.array
                            row
                            (setElement r col val)))

getRow : Board -> Int -> Maybe (Array Int)
getRow board row =
  if row<0 || row>=board.rows then
    Nothing
  else
    Array.get row board.array
