----------------------------------------------------------------------
--
-- DebuggingRender.elm
-- The old table rendering code that showed just the goal
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module DebuggingRender exposing (renderBoard)

import Styles.Page exposing (id, class, PId(..), PClass(..))
import Board exposing(Board)
import PuzzleDB
import Entities exposing (nbsp, copyright)

import Array exposing (Array)
import Char
import String

import Html exposing
  (Html, Attribute, button, div, h2, text, table, tr, td, th
  ,input, button, br, a, img)
import Html.Attributes
  exposing (style, align, value, size, href, src, title, alt, width, height)

showDebugNumbers : Bool
showDebugNumbers = True

renderElement : Int -> Html a
renderElement val =
  td
    [ class BoardCellClass ]
    [ text (if val == 0 then nbsp else toString val) ]

renderRow : Int -> Array Int -> Html a
renderRow rowNum row =
  let es = (List.map renderElement (Array.toList row))
      elts = if showDebugNumbers then
               (debugNumbersIntElement rowNum) :: es
             else
               es
  in
      tr [] elts                         

debugNumbersElement : String -> Html a
debugNumbersElement label =
  td [ class BoardLabelClass ] [ text label ]

debugNumbersIntElement : Int -> Html a
debugNumbersIntElement num =
  debugNumbersElement (toString num)                              

debugNumbersTopRow : (Board Int) -> Html a
debugNumbersTopRow board =
  tr
    []
    (debugNumbersElement nbsp ::
       List.map debugNumbersIntElement [0..(board.cols-1)])

renderBoard : (Board Int) -> Html a
renderBoard board =
  let rs = (List.map2 renderRow [0..(board.cols-1)] (Array.toList board.array))
      rows = if showDebugNumbers then
               (debugNumbersTopRow board) :: rs
             else
               rs
  in
      table
        [ id BoardId ]
        rows
