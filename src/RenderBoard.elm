----------------------------------------------------------------------
--
-- RenderBoard.elm
-- Render the game board.
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module RenderBoard exposing ( makeGameState
                            , render
                            , renderKeypad
                            )

import SharedTypes exposing (GameState
                            , IntBoard
                            , BClassMatrix, BClassBoard
                            , Labels, LabelsBoard
                            , Hints, HintsBoard
                            , Msg (ClickCell, PressKey)
                            )
import Styles.Board exposing (class, classes, BClass(..))
import Board exposing(Board, get, set)
import PuzzleDB
import Entities exposing (nbsp, copyright)
import Events exposing (onClickWithId, onClickWithInt)
import SimpleMatrix exposing (Matrix, Location, loc, row, col, incRowBy, incColBy)

import Array exposing (Array)
import Char
import String
import List exposing (map)
import List.Extra as LE

import Debug exposing (log)

import Json.Decode as Json

import Html exposing
  (Html, Attribute, div, text, table, tr, td, th, a, img, button)
import Html.Attributes
  exposing (style, value, href, src, title, alt, id, autofocus)
import Html.Events exposing (on)

br : Html a
br =
  Html.br [][]

cellId : Int -> Int -> Attribute m
cellId row col =
  id ((toString row) ++ "," ++ (toString col))

classedCell : Int -> Int -> Int -> List BClass -> Html Msg
classedCell num row col classTypes =
  td [ classes <| CellTd :: classTypes
     , cellId row col
     , onClickWithId ClickCell
     ]
    [ text <| toString num ]

cell : Bool -> Int -> Int -> Int -> Html Msg
cell isSelected num row col =
  classedCell num row col (if isSelected then [ Selected ] else [])

emptyCell : Html a
emptyCell =
  td [ classes [ CellTd, EmptyCellBackground ] ]
    [ div [ class EmptyCell ]
        [ text nbsp ]
    ]

unfilledCell : Bool -> Int -> Int -> Html Msg
unfilledCell isSelected row col =
  let cls = if isSelected then
              classes [CellTd, Selected]
            else
              class CellTd
  in
      td [ cls
         , onClickWithId ClickCell
         ]
      [ div [ class UnfilledCell
            , cellId row col
            ]
          [ text nbsp ]
    ]

errorCell : Int -> Int -> Int -> Html Msg
errorCell num row col =
  classedCell num row col [ Error ]

selectedCell : Int -> Int -> Int -> Html Msg
selectedCell num row col =
  classedCell num row col [ Selected ]

selectedErrorCell : Int -> Int -> Int -> Html Msg
selectedErrorCell num row col =
  classedCell num row col [ SelectedError ]

hintChars : List Int -> Int -> String
hintChars hints hint =
  String.append (if (List.member hint hints) then toString hint else nbsp)
                nbsp

hintRow : Int -> Int -> List Int -> Html a
hintRow min max hints =
  [min .. max]
  |> map (hintChars hints)
  |> String.concat
  |> String.left (2*(max-min)+1)
  |> text

hintCell : List Int -> Int -> Int -> Html a
hintCell hints row col =
  td [ cellId row col ]
    [ div [ class Hint ]
        [ hintRow 1 3 hints
        , br
        , hintRow 4 6 hints
        , br
        , hintRow 7 9 hints
        ]
    ]

labelCell : Int -> Int -> Html a
labelCell right bottom =
  td []
    [ div [ class Label ]
        [ table [ class LabelTable ]
            [ tr [ class LabelTr ]
                [ td [ class LabelTd ] [ text nbsp ]
                , td [ class RightLabelTd ]
                  [ text <| if right == 0 then nbsp else (toString right) ]
                ]
            , tr [ class LabelTr ]
              [ td [ class BottomLabelTd ]
                  [ text <| if bottom == 0 then nbsp else (toString bottom) ]
              , td [ class LabelTd ] [ text nbsp ]
              ]
            ]
        ]
    ]

emptyLabels : Labels
emptyLabels = (0, 0)

emptyHints : Hints
emptyHints = []

sumColLoop : Int -> Int -> Int -> (IntBoard) -> Int
sumColLoop row col sum board =
  let elt = get row (col-1) board
  in
      if elt == 0 then
        sum
      else
        sumColLoop (row+1) col (sum + elt) board

sumCol : Int -> Int -> IntBoard -> Int
sumCol row col board =
  sumColLoop row col 0 board

sumRowLoop : Int -> Int -> Int -> (IntBoard) -> Int
sumRowLoop row col sum board =
  let elt = get (row-1) col board
  in
      if elt == 0 then
        sum
      else
        sumRowLoop row (col+1) (sum + elt) board    

sumRow : Int -> Int -> IntBoard -> Int
sumRow row col board =
  sumRowLoop row col 0 board

computeLabel : Int -> Int -> LabelsBoard -> IntBoard -> LabelsBoard
computeLabel row col res board =
  if (get (row-1) (col-1) board) /= 0 then
    res
  else
    let rowsum = sumRow row col board
        colsum = sumCol row col board
    in
        if rowsum==0 && colsum == 0 then
          res
        else
          set row col (rowsum, colsum) res

computeLabelsColsLoop : Int -> Int -> LabelsBoard -> IntBoard -> LabelsBoard
computeLabelsColsLoop row col res board =
  if col >= res.cols then
    res
  else
    computeLabelsColsLoop row
                          (col+1)
                          (computeLabel row col res board)
                          board

computeLabelsCols : Int -> LabelsBoard -> IntBoard -> LabelsBoard
computeLabelsCols row res board =
  computeLabelsColsLoop row 0 res board

computeLabelsRowLoop : Int -> LabelsBoard -> IntBoard -> LabelsBoard
computeLabelsRowLoop row res board =
  if row >= res.rows then
    res
  else
    computeLabelsRowLoop (row+1)
                         (computeLabelsCols row res board)
                         board

computeLabelsRow : Int -> IntBoard -> LabelsBoard
computeLabelsRow row board =
  let res = Board.make (board.rows+1) (board.cols+1) emptyLabels
  in
      computeLabelsRowLoop row res board

computeLabels : IntBoard -> LabelsBoard
computeLabels board =
  computeLabelsRow 0 board

renderFilledCell : Bool -> Int -> Int -> Int -> GameState -> Html Msg
renderFilledCell isSelected num row col state =
  let maybeClass = Board.get (row-1) (col-1) state.cellClasses
      classes = case maybeClass of
                    Nothing -> []
                    Just a -> [ a ]
      selectedClasses = case maybeClass of
                            Just Error -> [ SelectedError ]
                            _ -> Selected :: classes
      classes2 = if isSelected then selectedClasses else []
  in
      classedCell num row col classes2

renderCell : Int -> Int -> GameState -> Html Msg
renderCell row col state =
  let labels = state.labels
      (right, bottom) = get row col labels
      bRow = row - 1
      bCol = col - 1
      selection = state.selection
      isSelected = case selection of
                     Nothing -> False
                     Just x -> x == (bRow, bCol)
  in
      if right==0 && bottom==0 then
        let val = (get bRow bCol state.guesses)
        in
            if val == 0 then
              if (get bRow bCol state.board) == 0 then
                emptyCell
              else
                unfilledCell isSelected bRow bCol
            else
              renderFilledCell isSelected val bRow bCol state
      else
        labelCell right bottom               

renderColsLoop : Int -> Int -> List (Html Msg) -> GameState -> List (Html Msg)
renderColsLoop row col res state =
  if col >= state.labels.cols then
    List.reverse res
  else
    renderColsLoop
      row
      (col+1)
      (renderCell row col state :: res)
      state
          
renderCols : Int -> GameState -> List (Html Msg)
renderCols row state =
  renderColsLoop row 0 [] state

renderRow : Int -> GameState -> Html Msg
renderRow row state =
   tr [] <| renderCols row state

renderRowsLoop : Int -> List (Html Msg) -> GameState -> List (Html Msg)
renderRowsLoop row res state =
  if row >= state.labels.rows then
    List.reverse res
  else
    renderRowsLoop (row+1)
                   (renderRow row state :: res)
                   state

-- If acc contains no zeroes, and doesn't sum to res,
-- Mark (row,col) and List.len acc back from there as Errors
markBadSumColSegment : Int -> Int -> Int -> List Int -> BClassBoard -> BClassBoard
markBadSumColSegment row col sum acc res =
  markBadSumSegment row col sum acc SimpleMatrix.updateColRangeWithValue res

-- If guess is in acc, then mark both (row,col) and (row,col - (position guess acc))
-- as Errors
markFilledColDuplicates : Int -> Int -> Int -> List Int -> BClassBoard -> BClassBoard
markFilledColDuplicates guess rowidx colidx acc res =
  markFilledDuplicates
    guess rowidx colidx (\i -> incRowBy <| negate i) acc res

-- If row is zero, call computedFilledColClasses to add the
-- display classes to res for the given col.
-- If col >= cols, and all elements of acc are non-zero, add Just Error
-- to all of the corresponding elements of res.
-- Otherwise, if the guess at (row,col) is already in acc, add Just Error to res.
-- Finally, recurse with col+1.
computeFilledColClassesRows : Int -> Int -> Int -> IntBoard -> IntBoard -> Int -> List Int -> BClassBoard -> BClassBoard
computeFilledColClassesRows row col rows board guesses sum acc res =
  let val = Board.get row col board
      guess = Board.get row col guesses
      res2 = if sum > 0 && guess /= 0 then
               markFilledColDuplicates guess row col acc res
             else
               res
      res3 = if val == 0 && sum > 0 then
               markBadSumColSegment row col sum acc res2
             else
               res3
      sum2 = if val == 0 then 0 else (sum + val)
      acc2 = if val == 0 then [] else (guess :: acc)
  in
      if row >= rows then
        res3
      else
        computeFilledColClassesRows
          (row+1) col rows board guesses sum2 acc2 res3

-- Add to res the display class for each of the guesses.
computeFilledColClasses : Int -> IntBoard -> IntBoard -> BClassBoard -> BClassBoard
computeFilledColClasses col board guesses res =
  computeFilledColClassesRows 0 col board.rows board guesses 0 [] res

type alias BClassMatrixRangeUpdater =
  Location -> Int -> Maybe BClass -> BClassMatrix -> BClassMatrix

markBadSumSegment : Int -> Int -> Int -> List Int -> BClassMatrixRangeUpdater -> BClassBoard -> BClassBoard
markBadSumSegment row col sum acc updater res =
  if List.member 0 acc then
    res
  else
    let accsum = List.foldr (+) 0 acc
    in
        if accsum == sum then
          res
        else
          { res | array =
              updater (row, col) ((List.length acc) + 1) (Just Error) res.array
          }

-- If acc contains no zeroes, and doesn't sum to res,
-- Mark (row,col) and List.len acc back from there as Errors
markBadSumRowSegment : Int -> Int -> Int -> List Int -> BClassBoard -> BClassBoard
markBadSumRowSegment row col sum acc res =
  markBadSumSegment row col sum acc SimpleMatrix.updateRowRangeWithValue res

markFilledDuplicates : Int -> Int -> Int -> (Int -> Location -> Location) -> List Int -> BClassBoard -> BClassBoard
markFilledDuplicates guess row col incrementor acc res =
  if guess == 0 then --Shouldn't happen. Famous last words.
    res
  else
    let idx = LE.elemIndex guess acc
    in
        case idx of
            Nothing -> res
            Just i ->
              let (row', col') = incrementor i (row, col)
              in
                  Board.set row col (Just Error) res
                    |> Board.set row' col' (Just Error)

-- If guess is in acc, then mark both (row,col) and (row,col - (position guess acc))
-- as Errors
markFilledRowDuplicates : Int -> Int -> Int -> List Int -> BClassBoard -> BClassBoard
markFilledRowDuplicates guess rowidx colidx acc res =
  markFilledDuplicates
    guess rowidx colidx (\i -> incColBy <| negate i) acc res

-- If row is zero, call computedFilledColClasses to add the
-- display classes to res for the given col.
-- If col >= cols, and all elements of acc are non-zero, add Just Error
-- to all of the corresponding elements of res.
-- Otherwise, if the guess at (row,col) is already in acc, add Just Error to res.
-- Finally, recurse with col+1.
computeFilledRowClassesCols : Int -> Int -> Int -> IntBoard -> IntBoard -> Int -> List Int -> BClassBoard -> BClassBoard
computeFilledRowClassesCols row col cols board guesses sum acc res =
  let res2 = if row == 0 && col < cols then
               computeFilledColClasses col board guesses res
             else
               res
      val = Board.get row col board
      guess = Board.get row col guesses
      res3 = if sum > 0 && guess /= 0 then
               markFilledRowDuplicates guess row col acc res2
             else
               res2
      res4 = if val == 0 && sum > 0 then
               markBadSumRowSegment row col sum acc res3
             else
               res3
      sum2 = if val == 0 then 0 else (sum + val)
      acc2 = if val == 0 then [] else (guess :: acc)
  in
      if col >= cols then
        res4
      else
        computeFilledRowClassesCols
          row (col+1) cols board guesses sum2 acc2 res4
                             
-- Add to res the display class for each of the guesses in the given row.
-- Then recurse for the next row, until row >= rows.
computeFilledRowClassesLoop : Int -> Int -> IntBoard -> IntBoard -> BClassBoard -> BClassBoard
computeFilledRowClassesLoop row rows board guesses res =
  if row >= rows then
    res
  else
    let res2 = computeFilledRowClassesCols row 0 board.cols board guesses 0 [] res
    in
        computeFilledRowClassesLoop (row+1) rows board guesses res2

-- Add to res the display class for each of the guesses.
computeFilledRowClasses : IntBoard -> IntBoard -> BClassBoard -> BClassBoard
computeFilledRowClasses board guesses res =
  computeFilledRowClassesLoop 0 board.rows board guesses res

-- Returns a Board containing a Maybe BClass for rendering each cell of
-- the board. It will be Nothing for cells that have no guess.
computeFilledCellClasses : IntBoard -> IntBoard -> BClassBoard
computeFilledCellClasses board guesses =
  Board.make board.rows board.cols Nothing
    |> computeFilledRowClasses board guesses

makeGameState : IntBoard -> GameState
makeGameState board =
  let rows = board.rows
      cols = board.cols
      guesses = Board.make rows cols board.default
      cellClasses = Board.make rows cols Nothing --computed by renderRows
      hints = Board.make rows cols emptyHints
  in
      { board = board
      , labels = (computeLabels board)
      , cellClasses = cellClasses
      , guesses = guesses
      , hints = hints
      , selection = Nothing
      }

renderRows : GameState -> List (Html Msg)
renderRows state =
  let -- cellClasses = computeFilledCellClasses state.board state.guesses
      state2 = state -- { state | cellClasses = log "cellClasses" cellClasses }
  in
      renderRowsLoop 0 [] state2

render : GameState -> Html Msg
render state =
  div []
    [ Styles.Board.style
    , table [ class Table ]
        (renderRows state)
    ]

--
-- The push-button keypad
--

keycodeCell : Int -> String -> Html Msg
keycodeCell keycode label =
  td [ class KeypadTd
     , onClickWithInt PressKey keycode
     ]
    [ button [ class KeypadButton
             , autofocus (label == " ")]
        [ text  label ]
    ]

keypadAlist : List (Char, Int)
keypadAlist =
  [ ('^', Char.toCode 'i')
  , ('v', Char.toCode 'k')
  , ('<', Char.toCode 'j')
  , ('>', Char.toCode 'l')
  , ('*', Char.toCode '*')
  , ('#', Char.toCode '#')
  , (' ', Char.toCode '0')
  ]

keypadKeycode : Char -> Int
keypadKeycode char =
  if char >= '0' && char <= '9' then
    Char.toCode char
  else
    let pair = LE.find (\x -> (fst x) == char) keypadAlist
    in
        case pair of
          Nothing ->
            0
          Just (_, res) ->
            res

renderKeypadCell : Char -> Html Msg
renderKeypadCell char =
  keycodeCell (keypadKeycode char) (String.fromList [char])

renderKeypadRow : String -> Html Msg
renderKeypadRow string =
  let chars = String.toList string
  in
      tr []
        <| List.map renderKeypadCell chars
-- 1 2 3 ^
-- 4 5 6 v
-- 7 8 9 <
-- * 0 # >
renderKeypad : Html Msg
renderKeypad =
  table [ class Table]
    [ Styles.Board.style
    , renderKeypadRow "123*"
    , renderKeypadRow "456#"
    , renderKeypadRow "78^ "
    , renderKeypadRow "9<v>"
    ]
