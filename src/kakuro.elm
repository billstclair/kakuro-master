----------------------------------------------------------------------
--
-- kakuro.elm
-- kakuro-master.com main screen
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

import Styles.Page exposing (id, class, PId(..), PClass(..))
import KakuroNative exposing (sha256)
import Board exposing(Board)
import PuzzleDB
import Entities exposing (nbsp, copyright)
import DebuggingRender
import RenderBoard

import Array exposing (Array)
import Char
import String
import Time exposing (Time, second)
import Random
import Task

import Html exposing
  (Html, Attribute, button, div, h2, text, table, tr, td, th
  ,input, button, a, img, span)
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

initialKind : Int
initialKind = 10

pageTitle : String
pageTitle = KakuroNative.setTitle "Kakuro Master"

type alias Model =
      { board : Board Int
      , kind : Int
      , index : Int
      , gencount : Int
      , seed : Maybe Random.Seed
      , time : Time
      }

seedCmd : Cmd Msg
seedCmd =
  Task.perform (\x -> Nop) (\x -> Seed x) Time.now

init : (Model, Cmd Msg)
init = (model, seedCmd)

defaultBoard : Board Int
defaultBoard =
  Board.make 6 6 0
    |> Board.set 6 7 9
    |> Board.set 1 2 5

model : Model
model =
  let (idx, board) = PuzzleDB.nextBoardOfKind initialKind 0
  in
      Model
        board         --board
        initialKind   --kind
        idx           --index
        0             --gencount
        Nothing       --seed
        0             --time

-- UPDATE

type Msg
  = Generate
  | Tick Time
  | Seed Time
  | Nop

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    Generate ->
      let (idx, board) = PuzzleDB.nextBoardOfKind model.kind model.index
      in
          ({model | index = idx, board = board, gencount = (model.gencount+1)},
           Cmd.none)
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

sqrimg : String -> String -> Int -> Html Msg
sqrimg url name size =
  img [ src url
      , title name
      , alt name
      , width size
      , height size ]
      []
          
logoLink : String -> String -> String -> Int -> Html Msg
logoLink url img name size =
  a [ href url ]
    [ sqrimg ("images/" ++ img) name size ]

showValue : a -> Html Msg
showValue seed =
  div [] [text <| toString seed]

br : Html a
br = Html.br [] []

mailLink : String -> Html Msg
mailLink email =
  span []
    [ text "<"
    , a [ href ("mailto: " ++ email) ]
      [ text email ]
    , text ">"
    ]

space : Html Msg
space = text " "

view : Model -> Html Msg
view model =
  div [ align "center" --deprecated, so sue me
      ]
    [ Styles.Page.style
    , h2 [] [text pageTitle]
    , div
        [ id TopInputId ]
        [ button [ onClick Generate
                 , class ControlsClass ]
           [ text "Next" ]
        , br
        , text "Board Number: "
        , text (toString model.index)
        , br
        -- , text ("sha256(\"foo\"): " ++ sha256("foo"))
        -- , text (" " ++ toString model.time)  -- Will eventually be timer
        -- , showValue model.seed               -- debugging
        ]
    , div [] [ RenderBoard.render model.board ]
    , div
        [ id FooterId ]
        [ text (copyright ++ " 2016 Bill St. Clair ")
        , mailLink "billstclair.com"
        , br
        , logoLink "https://steemit.com/created/kakuro-master"
            "steemit-icon-114x114.png" "Steemit articles" 32
        , space
        , logoLink "https://github.com/billstclair/kakuro-master"
            "GitHub-Mark-32px.png" "GitHub source code" 32
        , space
        , logoLink "http://elm-lang.org/"
            "elm-logo-125x125.png" "Elm inside" 28
        ]
        
    ]
