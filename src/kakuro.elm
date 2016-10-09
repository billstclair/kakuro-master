----------------------------------------------------------------------
--
-- kakuro.elm
-- kakuro main screen
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

import KakuroStylesheet exposing (id, class, KId(..), KClass(..))
import KakuroNative
import Board exposing(Board)
import Generate

import Array exposing (Array)
import Char
import String
import Time exposing (Time, second)
import Random
import Task

import Html exposing
  (Html, Attribute, button, div, h2, text, table, tr, td, th
  ,input, button, br, a, img)
import Html.Attributes
  exposing (style, align, value, size, href, src, title, alt, width, height)
import Html.App as Html
import Html.Events exposing (onClick, onInput)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

initialRows : Int
initialRows = 9

initialCols: Int
initialCols = 9

initialMaxrun : Int
initialMaxrun = 4

pageTitle : String
pageTitle = KakuroNative.setTitle "Kakuro Master"

type alias Model =
  { board : Board
  , maxrun : Int
  , gencount : Int
  , seed : Maybe Random.Seed
  , time : Time
  }

seedCmd : Cmd Msg
seedCmd =
  Task.perform (\x -> Nop) (\x -> Seed x) Time.now

init : (Model, Cmd Msg)
init = (model, seedCmd)

initialBoard : Board
initialBoard =
  Board.make initialRows initialCols
    |> Board.set 6 7 9
    |> Board.set 1 2 5

model : Model
model =
  Model
    initialBoard  --board
    initialMaxrun --maxrun
    0             --gencount
    Nothing       --seed
    0             --time

-- UPDATE

type Msg
  = Generate
  | SetMaxrun String
  | Tick Time
  | Seed Time
  | Nop

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    Generate ->
      (model, Cmd.none)
    SetMaxrun mr ->
      case String.toInt mr of
          Err _ -> (model, Cmd.none)
          Ok mri ->
            let m = max 3 (if mri >= 10 then mri % 10 else mri)
              in ({ model | maxrun = m }, Cmd.none)
    Tick time ->
      ({model | time = model.time + 1}, Cmd.none)
    Seed time ->
      ({model | seed = Just <| Random.initialSeed (round time)}, Cmd.none)
    Nop ->
      (model, Cmd.none)
          
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

-- VIEW

showDebugNumbers : Bool
showDebugNumbers = True

nbsp : String
nbsp = String.cons (Char.fromCode 160) "" -- \u00A0

renderElement : Int -> Html Msg
renderElement val =
  td
    [ class BoardCellClass ]
    [ text (if val == 0 then nbsp else toString val) ]

renderRow : Int -> Array Int -> Html Msg
renderRow rowNum row =
  let es = (List.map renderElement (Array.toList row))
      elts = if showDebugNumbers then
               (debugNumbersIntElement rowNum) :: es
             else
               es
  in
      tr [] elts                         

debugNumbersElement : String -> Html Msg
debugNumbersElement label =
  td [ class BoardLabelClass ] [ text label ]

debugNumbersIntElement : Int -> Html Msg
debugNumbersIntElement num =
  debugNumbersElement (toString num)                              

debugNumbersTopRow : Board -> Html Msg
debugNumbersTopRow board =
  tr
    []
    (debugNumbersElement nbsp ::
       List.map debugNumbersIntElement [0..(board.cols-1)])

renderBoard : Board -> Html Msg
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

sqrimg : String -> String -> Int -> Html Msg
sqrimg url name size =
  img [ src url
      , title name
      , alt name
      , width size
      , height size ]
      []
          
showValue : a -> Html Msg
showValue seed =
  div [] [text <| toString seed]

view : Model -> Html Msg
view model =
  div [ align "center" --deprecated, so sue me
      ]
    [ KakuroStylesheet.style
    , h2 [] [text pageTitle]
    , div
        [ id TopInputId ]
        [input [ value <| toString model.maxrun
               , size 1
               , onInput SetMaxrun
               , class ControlsClass ] []
        , text " "
        , button [ onClick Generate
                 , class ControlsClass ]
           [ text "Generate" ]
        -- , text (" " ++ toString model.time)  -- Will eventually be timer
        -- , showValue model.seed               -- debugging
        ]
    , div [] [ renderBoard model.board ]
    , div
        [ id FooterId ]
        [ a [ href "https://steemit.com/created/kakuro-master" ]
          [ sqrimg "images/steemit-icon-114x114.png" "Steemit articles" 32 ]
        , text " "
        , a [ href "https://github.com/billstclair/kakuro-master" ]
          [ sqrimg "images/GitHub-Mark-32px.png" "GitHub source code" 32 ]
        , text " "
        , a [ href "http://elm-lang.org/" ]
          [ sqrimg "images/elm-logo-125x125.png" "Elm inside" 28 ]
      ]
    ]
