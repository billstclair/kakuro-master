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

module BoardSize
    exposing
        ( computeBoardSizes
        , Rect
        , cellRect
        , cellTextLocation
        , insetRectForSelection
        , bottomLabelLocation
        , rightLabelLocation
        , labelBackgroundRect
        , hintTextLocation
        , iosTopPad
        )

import SharedTypes exposing (Model, BoardSizes, Platform(..))
import Window
import Debug exposing (log)

cellBorder : Int
cellBorder =
    1

labelBackgroundInset : Int
labelBackgroundInset =
    1

selectionBorder : Int
selectionBorder =
    3

whiteSpace : Int
whiteSpace =
    1

minimumCellSize : Int
minimumCellSize =
    20

maximumCellSize : Int
maximumCellSize =
    80

minimumFontSize : Int
minimumFontSize =
    8

minimumPossibilitiesHeight : Int
minimumPossibilitiesHeight =
    60

keypadRows : Int
keypadRows =
    4

maxKeypadSize : Int
maxKeypadSize =
    300

minKeypadSize : Int -> Int
minKeypadSize windowHeight =
    1 * windowHeight // 4

defaultWindowSize : Window.Size
defaultWindowSize =
    { width = 1024, height = 768 }

getWindowSize : Model -> Window.Size
getWindowSize model =
    case model.windowSize of
      Nothing -> defaultWindowSize
      Just ws -> ws

computeCellSize : Model -> Int
computeCellSize model =
    let windowSize = getWindowSize model
        rows = model.kind + 1
        h = windowSize.height
        maxSize = h - (nonBoardSize model) - (min maxKeypadSize <| minKeypadSize h)
        total = min (windowSize.width - 10) maxSize
        size = (total - 2 * (cellBorder + whiteSpace)) // rows
    in
        size

boardFromCellSize : Model -> Int -> Int
boardFromCellSize model cellSize =
    (cellSize * (model.kind + 1)) + (cellBorder + whiteSpace)

iosTopPad : Model -> Int
iosTopPad model =
    case model.platform of
        IosPlatform -> 25
        AndroidPlatform -> 10
        _ -> 0

nonBoardSize : Model -> Int
nonBoardSize model =
    let size = case model.platform of
                   AndroidPlatform -> 110
                   _ -> 130
    in
        size + (iosTopPad model)

computeBoardSize : Model -> Int
computeBoardSize model =
    computeCellSize model
        |> boardFromCellSize model

computeBoardSizes : Model -> BoardSizes
computeBoardSizes model =
    let cellSize = computeCellSize model
        boardSize = boardFromCellSize model cellSize
        cellFontSize = 3 * cellSize // 4
        labelFontSize = 5 * cellSize // 12
        hintFontSize = cellSize // 3
        windowSize = getWindowSize model
        h = windowSize.height
        keypadSize = h - boardSize - (nonBoardSize model)
    in
        { boardSize = boardSize
        , cellSize = (log "cellSize" cellSize)
        , cellFontSize = cellFontSize
        , labelFontSize = labelFontSize
        , hintFontSize = hintFontSize
        , keypadSize = keypadSize
        , keypadFontSize = (7 * keypadSize) // (keypadRows * 8)
        }

type alias Rect =
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    }

cellRect : Int -> Int -> BoardSizes -> Rect
cellRect row col sizes =
    let cellSize = sizes.cellSize
        offset = cellBorder + whiteSpace
        y = row * cellSize + offset
        x = col * cellSize + offset
        w = cellSize - offset
        h = cellSize - offset
    in
        { x = x, y = y, w = w, h = h }

insetRectForSelection : Rect -> Rect
insetRectForSelection rect =
    let delta = selectionBorder - cellBorder - 1
        x = rect.x + delta
        y = rect.y + delta
        w = rect.w - 2 * delta
        h = rect.h - 2 * delta
    in
        { x = x, y = y, w = w, h = h }

cellTextLocation : Rect -> ( Int, Int )
cellTextLocation cellRect =
    ( cellRect.x + (cellRect.w * 30 // 97)
    , cellRect.y + (cellRect.h * 72 // 97)
    )

bottomLabelLocation : Rect -> ( Int, Int )
bottomLabelLocation cellRect =
    ( cellRect.x + (cellRect.w * 8 // 97)
    , cellRect.y + (cellRect.h * 86 // 97)
    )

rightLabelLocation : Rect -> ( Int, Int )
rightLabelLocation cellRect =
    ( cellRect.x + (cellRect.w * 50 // 97)
    , cellRect.y + (cellRect.h * 40 // 97)
    )

hintToRow : Int -> Int
hintToRow hint =
    if hint < 4 then
        0
    else if hint < 7 then
        1
    else
        2

hintToCol : Int -> Int
hintToCol hint =
    (hint - 1) % 3

hintColToTextX : Int -> Int
hintColToTextX hintCol =
    if hintCol == 0 then
        13
    else if hintCol == 1 then
        40
    else
        67

hintRowToTextY : Int -> Int
hintRowToTextY hintRow =
    if hintRow == 0 then
        32
    else if hintRow == 1 then
        60
    else
        89

hintTextLocation : Int -> Rect -> ( Int, Int )
hintTextLocation hint cellRect =
    let row = hintToRow hint
        col = hintToCol hint
        x = hintColToTextX col
        y = hintRowToTextY row
    in
        ( cellRect.x + (cellRect.w * x // 97)
        , cellRect.y + (cellRect.h * y // 97)
        )

labelBackgroundRect : Rect -> Rect
labelBackgroundRect rect =
    { x = rect.x + labelBackgroundInset
    , y = rect.y + labelBackgroundInset
    , w = rect.w - 2 * labelBackgroundInset
    , h = rect.h - 2 * labelBackgroundInset
    }
