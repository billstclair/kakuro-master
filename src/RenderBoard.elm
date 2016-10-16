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

module RenderBoard exposing ( IntBoard
                            , Labels, LabelsBoard
                            , Hints, HintsBoard
                            , GameState
                            , makeGameState
                            , render
                            )

import Styles.Board exposing (class, classes, BClass(..))
import Board exposing(Board, get, set)
import PuzzleDB
import Entities exposing (nbsp, copyright)

import Array exposing (Array)
import Char
import String
import List exposing (map)

import Debug exposing (log)

import Html exposing
  (Html, Attribute, div, text, table, tr, td, th, a, img)
import Html.Attributes
  exposing (style, value, href, src, title, alt)

br : Html a
br =
  Html.br [][]

classedCell : Int -> List BClass -> Html a
classedCell num classTypes =
  td [ classes <| CellTd :: classTypes ]
    [ text <| toString num ]

cell : Int -> Html a
cell num =
  classedCell num []

emptyCell : Html a
emptyCell =
  td [ class CellTd ]
    [ div [ class EmptyCell ]
        [ text nbsp ]
    ]

errorCell : Int -> Html a
errorCell num =
  classedCell num [ Error ]

selectedCell : Int -> Html a
selectedCell num =
  classedCell num [ Selected ]

selectedErrorCell : Int -> Html a
selectedErrorCell num =
  classedCell num [ SelectedError ]

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

hintCell : List Int -> Html a
hintCell hints =
  td []
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

type alias IntBoard =
  Board Int

type alias Labels =
  (Int, Int)

type alias LabelsBoard =
  Board Labels

type alias Hints =
  List Int

type alias HintsBoard =
  Board Hints

type alias GameState =
  { board : IntBoard
  , labels: LabelsBoard
  , guesses : IntBoard
  , hints : HintsBoard
  }

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

renderCell : Int -> Int -> GameState -> Html a
renderCell row col state =
  let labels = state.labels
      guesses = state.guesses
      (right, bottom) = get row col labels
  in
      if right==0 && bottom==0 then
        let val = get (row-1) (col-1) guesses
        in
            if val == 0 then
              emptyCell
            else
              cell val
      else
        labelCell right bottom               

renderColsLoop : Int -> Int -> List (Html a) -> GameState -> List (Html a)
renderColsLoop row col res state =
  if col >= state.labels.cols then
    List.reverse res
  else
    renderColsLoop
      row
      (col+1)
      (renderCell row col state :: res)
      state
          
renderCols : Int -> GameState -> List (Html a)
renderCols row state =
  renderColsLoop row 0 [] state

renderRow : Int -> GameState -> Html a
renderRow row state =
   tr [] <| renderCols row state

renderRowsLoop : Int -> List (Html a) -> GameState -> List (Html a)
renderRowsLoop row res state =
  if row >= state.labels.rows then
    List.reverse res
  else
    renderRowsLoop (row+1)
                   (renderRow row state :: res)
                   state

renderRows : GameState -> List (Html a)
renderRows state =
  renderRowsLoop 0 [] state

makeGameState : IntBoard -> GameState
makeGameState board =
  GameState
    board
    (computeLabels board)
    (Board.make board.rows board.cols board.default)
    (Board.make board.rows board.cols emptyHints)

render : GameState -> Html a
render state =
  div []
    [ Styles.Board.style
    , table [ class Table ]
        (renderRows state)
    ]
