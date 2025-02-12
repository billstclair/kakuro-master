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

import Array exposing (Array)
import Board exposing (Board)
import Char
import Entities exposing (copyright, nbsp)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , br
        , button
        , div
        , h2
        , img
        , input
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( align
        , alt
        , height
        , href
        , size
        , src
        , style
        , title
        , value
        , width
        )
import PuzzleDB
import String
import Styles.Page exposing (PClass(..), PId(..), class, id)


showDebugNumbers : Bool
showDebugNumbers =
    True


renderElement : Int -> Html a
renderElement val =
    td
        [ class BoardCellClass ]
        [ text
            (if val == 0 then
                nbsp

             else
                String.fromInt val
            )
        ]


renderRow : Int -> Array Int -> Html a
renderRow rowNum row =
    let
        es =
            List.map renderElement (Array.toList row)

        elts =
            if showDebugNumbers then
                debugNumbersIntElement rowNum :: es

            else
                es
    in
    tr [] elts


debugNumbersElement : String -> Html a
debugNumbersElement label =
    td [ class BoardLabelClass ] [ text label ]


debugNumbersIntElement : Int -> Html a
debugNumbersIntElement num =
    debugNumbersElement (String.fromInt num)


debugNumbersTopRow : Board Int -> Html a
debugNumbersTopRow board =
    tr
        []
        (debugNumbersElement nbsp
            :: List.map debugNumbersIntElement (List.range 0 <| board.cols - 1)
        )


renderBoard : Board Int -> Html a
renderBoard board =
    let
        rs =
            List.map2
                renderRow
                (List.range 0 <| board.cols - 1)
                (Array.toList board.array)

        rows =
            if showDebugNumbers then
                debugNumbersTopRow board :: rs

            else
                rs
    in
    table
        [ id BoardId ]
        rows
