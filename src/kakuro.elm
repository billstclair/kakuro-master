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

import Board exposing(Board)
import Generate

import Array exposing (Array)
import Char
import String

import Html exposing
  (Html, Attribute, button, div, text, table, tr, td, th, input, button, br, a, img)
import Html.Attributes
  exposing (style, align, value, size, href, src, title, alt, width, height)
import Html.App as Html
import Html.Events exposing (onClick, onInput)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

-- MODEL

initialRows : Int
initialRows = 9

initialCols: Int
initialCols = 9

initialMaxrun : Int
initialMaxrun = 4

type alias Model =
  { board : Board
  , rows : Int
  , cols : Int
  , maxrun : Int
  , gencount : Int
  }

initialBoard : Board
initialBoard =
  Board.set
    6 7 9
    (Board.set
       1 2 5
       (Board.make initialRows initialCols))

model : Model
model =
  Model
    initialBoard
    initialRows
    initialCols
    initialMaxrun
    0

-- UPDATE

type Msg
  = Generate
  | SetMaxrun String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Generate ->
      model
    SetMaxrun mr ->
      case String.toInt(mr) of
          Err _ -> model
          Ok mri ->
            let m = max 3 (if mri >= 10 then mri % 10 else mri)
              in { model | maxrun = m }

-- VIEW

showDebugNumbers : Bool
showDebugNumbers = True

tdStyle : List (Attribute Msg)
tdStyle = [ style [ ("padding", "4px")
                  , ("textAlign", "center")
                  , ("border", "1px solid black") ] ]

nbsp : String
nbsp = String.cons (Char.fromCode 160) "" -- \u00A0

renderElement : Int -> Html Msg
renderElement val =
  td
    tdStyle
    [ text (if val == 0 then nbsp else (toString val)) ]

renderRow : Int -> Array Int -> Html Msg
renderRow rowNum row =
  let es = (List.map renderElement (Array.toList row))
      elts = if showDebugNumbers then
               (debugNumbersIntElement rowNum) :: es
             else
               es
  in
      tr [] elts                         

dnStyle : List (Attribute Msg)
dnStyle = [ style [ ("padding", "4px")
                  , ("font-size", "50%")
                  , ("textAlign", "center")
                  , ("border", "1px solid black") ] ]

debugNumbersElement : String -> Html Msg
debugNumbersElement label =
  td dnStyle [ text label ]

debugNumbersIntElement : Int -> Html Msg
debugNumbersIntElement num =
  debugNumbersElement (toString num)                              

debugNumbersTopRow : Board -> Html Msg
debugNumbersTopRow board =
  tr
    []
    ((debugNumbersElement nbsp) ::
       (List.map debugNumbersIntElement [0..(board.cols-1)]))

renderBoard : Board -> Html Msg
renderBoard board =
  let rs = (List.map2 renderRow [0..(board.cols-1)] (Array.toList board.array))
      rows = if showDebugNumbers then
               (debugNumbersTopRow board) :: rs
             else
               rs
  in
      table
        [ style [ ("font-family", "\"Lucida Console\", Monaco, monospace")
                , ("font-size", "18pt")
                , ("padding", "2px")
                , ("border", "1px solid black")
                ]
        ]
        rows

sqrimg : String -> String -> Int -> Html Msg
sqrimg url name size =
  img [ src url
      , title name
      , alt name
      , width size
      , height size ]
      []
          
view : Model -> Html Msg
view model =
  div [ align "center" --deprecated, so sue me
      , style [ ("margin-top", "5em") ] ]
    [ div [ style [ ("margin-bottom", "0.5em") ] ]
        [input [ value (toString model.maxrun)
               , size 1
               , onInput SetMaxrun
               , style [ ("font-size", "14pt") ] ] []
        , text " "
        , button [ onClick Generate
                 , style [ ("font-size", "14pt") ] ]
           [ text "Generate" ]
        ]
    , div [] [ renderBoard model.board
             , div [ style [ ("margin-top", "2em") ] ]
               [ a [ href "https://github.com/billstclair/kakuro-master" ]
                   [ sqrimg "images/GitHub-Mark-32px.png" "GitHub" 32 ]
               , text " "
               , a [ href "https://steemit.com/created/kakuro-master" ]
                   [ sqrimg "images/steemit-icon-114x114.png" "Steemit" 32 ]
               ]
             ]
    ]
