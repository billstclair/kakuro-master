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

type alias Labels =
  (Int, Int)

type alias LabelsBoard =
  Board Labels

emptyLabels : Labels
emptyLabels = (0, 0)

sumColLoop : Int -> Int -> Int -> (Board Int) -> Int
sumColLoop row col sum board =
  let elt = get row (col-1) board
  in
      if elt == 0 then
        sum
      else
        sumColLoop (row+1) col (sum + elt) board

sumCol : Int -> Int -> Board Int -> Int
sumCol row col board =
  sumColLoop row col 0 board

sumRowLoop : Int -> Int -> Int -> (Board Int) -> Int
sumRowLoop row col sum board =
  let elt = get (row-1) col board
  in
      if elt == 0 then
        sum
      else
        sumRowLoop row (col+1) (sum + elt) board    

sumRow : Int -> Int -> Board Int -> Int
sumRow row col board =
  sumRowLoop row col 0 board

computeLabel : Int -> Int -> LabelsBoard -> Board Int -> LabelsBoard
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

computeLabelsColsLoop : Int -> Int -> LabelsBoard -> Board Int -> LabelsBoard
computeLabelsColsLoop row col res board =
  if col >= res.cols then
    res
  else
    computeLabelsColsLoop row
                          (col+1)
                          (computeLabel row col res board)
                          board

computeLabelsCols : Int -> LabelsBoard -> Board Int -> LabelsBoard
computeLabelsCols row res board =
  computeLabelsColsLoop row 0 res board

computeLabelsRowLoop : Int -> LabelsBoard -> Board Int -> LabelsBoard
computeLabelsRowLoop row res board =
  if row >= res.rows then
    res
  else
    computeLabelsRowLoop (row+1)
                         (computeLabelsCols row res board)
                         board

computeLabelsRow : Int -> Board Int -> LabelsBoard
computeLabelsRow row board =
  let res = Board.make (board.rows+1) (board.cols+1) emptyLabels
  in
      computeLabelsRowLoop row res board

computeLabels : Board Int -> LabelsBoard
computeLabels board =
  computeLabelsRow 0 board

renderCell : Int -> Int -> Board Int -> LabelsBoard -> Html a
renderCell row col board labelsBoard =
  let (right, bottom) = get row col labelsBoard
  in
      if right==0 && bottom==0 then
        let val = get (row-1) (col-1) board
        in
            if val == 0 then
              emptyCell
            else
              cell val
      else
        labelCell right bottom               

renderColsLoop : Int -> Int -> List (Html a) -> Board Int -> LabelsBoard -> List (Html a)
renderColsLoop row col res board labelsBoard =
  if col >= labelsBoard.cols then
    List.reverse res
  else
    renderColsLoop row
                   (col+1)
                   (renderCell row col board labelsBoard :: res)
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
    List.reverse res
  else
    renderRowsLoop (row+1)
                   (renderRow row board labelsBoard :: res)
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
