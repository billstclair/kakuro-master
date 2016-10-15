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

module RenderBoard exposing (render)

import Styles.Board exposing (class, classes, BClass(..))
import Board exposing(Board)
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
  td [ classes (CellTd :: classTypes) ]
    [ text (toString num) ]

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

render : Board Int -> Html a
render board =
  div []
    [ Styles.Board.style
    , table [ class Table ]
      [ tr []
          [ cell 1
          , label 26 17
          , cell 3
          , selectedErrorCell 4
          , cell 5
          , emptyCell
          , selectedCell 7
          , cell 8
          , errorCell 9
          ]
      , tr []
         (map cell [1..9])
      , tr []
          ( cell 1
            ::
            hintCell [3, 5, 7, 8]
            ::
            (map cell [3..9])
          )
      , tr []
          (map cell [1..9])
      ]
    ]
