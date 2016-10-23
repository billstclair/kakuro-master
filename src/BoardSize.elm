----------------------------------------------------------------------
--
-- BoardSize.elm
-- Size the elements of the board and keypad
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module BoardSize exposing (BoardSizes, computeBoardSizes)

import SharedTypes exposing ( Model )

import Window

cellBorder : Int
cellBorder = 1

selectionBorder : Int
selectionBorder = 3

whiteSpace : Int
whiteSpace = 2

minimumCellSize : Int
minimumCellSize = 20

maximumCellSize : Int
maximumCellSize = 100

minimumFontSize : Int
minimumFontSize = 8

minimumPossibilitiesHeight : Int
minimumPossibilitiesHeight = 60

keypadRows: Int
keypadRows = 4

defaultWindowSize : Window.Size
defaultWindowSize = { width = 1024, height = 768 }

computeCellSize : Model -> Int
computeCellSize model =
  let windowSize = case model.windowSize of
                       Nothing -> defaultWindowSize
                       Just ws -> ws
      kind = model.kind
      h = windowSize.height
      total = min windowSize.width <| h - ( 5 * h // ( kind + 5))
      size = (total - 2*(cellBorder + whiteSpace)) // kind 
  in
      max minimumCellSize size
        |> min maximumCellSize

boardFromCellSize : Model -> Int -> Int
boardFromCellSize model cellSize =
  (cellSize * model.kind) + 2*(cellBorder + whiteSpace)

computeBoardSize : Model -> Int
computeBoardSize model =
  computeCellSize model
    |> boardFromCellSize model
      
type alias BoardSizes =
  { boardSize : Int
  , cellSize : Int
  , cellFontSize : Int
  , labelFontSize : Int
  , hintFontSize : Int
  }

computeBoardSizes : Model -> BoardSizes
computeBoardSizes model =
  let cellSize = computeCellSize model
      boardSize = boardFromCellSize model cellSize
      cellFontSize = cellSize // 2
      labelFontSize = cellSize // 5
      hintFontSize = labelFontSize
  in
      { boardSize = boardSize
      , cellSize = cellSize
      , cellFontSize = cellFontSize
      , labelFontSize = labelFontSize
      , hintFontSize = hintFontSize
      }
                   
type alias Rect =
  { x : Int
  , y : Int
  , w : Int
  , h : Int
  }

type alias Location =
  { x : Int
  , y : Int
  }

cellRect : Int -> Int -> BoardSizes -> Rect
cellRect row col sizes =
  let cellSize = sizes.cellSize
      offset = cellBorder + whiteSpace
      x = row*cellSize + offset
      y = col*cellSize + offset
      w = cellSize - offset
      h = cellSize - offset
  in
      { x = x, y = y, w = w, h = h}

cellTextLocation : Rect -> Location
cellTextLocation cellRect =
  { x = cellRect.x + (cellRect.w * 37 // 97)
  , y = cellRect.y + (cellRect.h * 65 // 97)
  }

bottomLabelLocation : Rect -> Location
bottomLabelLocation cellRect =
  { x = cellRect.x + (cellRect.w * 25 // 97)
  , y = cellRect.y + (cellRect.h * 78 // 97)
  }
  
rightLabelLocation : Rect -> Location
rightLabelLocation cellRect =
  { x = cellRect.x + (cellRect.w * 60 // 97)
  , y = cellRect.y + (cellRect.h * 44 // 97)
  }

hintToRow : Int -> Int
hintToRow hint =
  if hint < 4 then
    0
  else if hint < 7 then
    1
  else
    2

hintRowToTextX : Int -> Int
hintRowToTextX  hintRow =
  if hintRow == 0 then
    12
  else if hintRow == 1 then
    42
  else
    72
hintRowToTextY : Int -> Int
hintRowToTextY  hintRow =
  if hintRow == 0 then
    27
  else if hintRow == 1 then
    57
  else
    87

hintTextLocation : Int -> Rect -> Location
hintTextLocation hint cellRect =
  let row = hintToRow hint
      x = hintRowToTextX row
      y = hintRowToTextY row
  in
      { x = cellRect.x + (cellRect.w * x // 97)
      , y = cellRect.y + (cellRect.w * y // 97)
      }
