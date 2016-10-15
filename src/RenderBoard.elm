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

module RenderBoard exposing (render, computeLabels)

import Styles.Board exposing (class, classes, BClass(..))
import Board exposing(Board, get, set)
import PuzzleDB
import Entities exposing (nbsp, copyright)

import Array exposing (Array)
import Char
import String
import List exposing (map)

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

label : Int -> Int -> Html a
label right bottom =
  td []
    [ div [ class Label ]
        [ table [ class LabelTable ]
            [ tr [ class LabelTr ]
                [ td [ class LabelTd ] [ text nbsp ]
                , td [ class RightLabelTd ] [ text (toString right) ]
                ]
            , tr [ class LabelTr ]
              [ td [ class BottomLabelTd ] [ text (toString bottom) ]
              , td [ class LabelTd ] [ text nbsp ]
              ]
            ]
        ]
    ]

type alias Labels =
  (Int, Int)

type alias LabelsBoard =
  Board Labels

emptyLabels : Labels
emptyLabels = (0, 0)

sumRowLoop : Int -> Int -> Int -> (Board Int) -> Int
sumRowLoop row col sum board =
  let elt = get row col board
  in
      if elt == 0 then
        sum
      else
        sumRowLoop row (col+1) (sum + elt) board    

sumColLoop : Int -> Int -> Int -> (Board Int) -> Int
sumColLoop row col sum board =
  let elt = get row col board
  in
      if elt == 0 then
        sum
      else
        sumColLoop (row+1) col (sum + elt) board

computeLabel : Int -> Int -> Board Int -> Labels
computeLabel row col board =
  let rowsum = sumRowLoop row col 0 board
      colsum = sumColLoop row col 0 board
  in
    if rowsum==0 && colsum==0 then
      emptyLabels
    else
      (rowsum, colsum)

computeLabelsLoop : Int -> Int -> Board Int -> LabelsBoard -> LabelsBoard
computeLabelsLoop row col board res =
  if (get (row-1) (col-1) board) == 0 then
    let res2 = set row col (computeLabel row col board) res
    in
        if col >= res2.cols then
          if row >= res2.rows then
            res2
          else
            computeLabelsLoop (row+1) 0 board res2
        else
          computeLabelsLoop row (col+1) board res2
  else
    res

computeLabels : Board Int -> LabelsBoard
computeLabels board =
  computeLabelsLoop
    0 0 board <| Board.make (board.rows+1) (board.cols+1) emptyLabels

renderCell : Int -> Int -> Board Int -> LabelsBoard -> Html a
renderCell row col board labelsBoard =
  cell 1

renderColsLoop : Int -> Int -> List (Html a) -> Board Int -> LabelsBoard -> List (Html a)
renderColsLoop row col res board labelsBoard =
  if col >= labelsBoard.cols then
    res
  else
    renderColsLoop row
                   (col+1)
                   ((renderCell row col board labelsBoard) :: res)
                   board
                   labelsBoard

renderCols : Int -> Board Int -> LabelsBoard -> List (Html a)
renderCols row board labelsBoard =
  renderColsLoop row 0 [] board labelsBoard

renderRow : Int -> Board Int -> LabelsBoard -> Html a
renderRow row board labelsBoard =
  tr [] <| renderCols row board labelsBoard

renderRowsLoop : Int -> List (Html a) -> Board Int -> LabelsBoard -> List (Html a)
renderRowsLoop row res board labelsBoard =
  if row >= labelsBoard.rows then
    res
  else
    renderRowsLoop (row+1)
                   ((renderRow row board labelsBoard) :: res)
                   board
                   labelsBoard
      

renderRows : Board Int -> LabelsBoard -> List (Html a)
renderRows board labelsBoard =
  renderRowsLoop 0 [] board labelsBoard

render : Board Int -> Html a
render board =
  div []
    [ Styles.Board.style
    , table [ class Table ]
        (renderRows board <| computeLabels board)
    ]
