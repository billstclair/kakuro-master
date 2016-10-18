----------------------------------------------------------------------
--
-- kakuro.elm
-- kakuro-dojo.com main screen
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

import SharedTypes exposing ( Model, Msg, Msg (..)
                            , IntBoard, HintsBoard, Selection, GameState)
import Styles.Page exposing (id, class, PId(..), PClass(..))
import KakuroNative exposing (sha256)
import Board exposing(Board)
import PuzzleDB
import Entities exposing (nbsp, copyright)
import DebuggingRender
import RenderBoard

import Array exposing (Array)
import Char
import List
import List.Extra as LE
import String
import Time exposing (Time, second)
import Random
import Task

import Debug exposing (log)

import Html exposing
  (Html, Attribute, button, div, p, h2, text, table, tr, td, th
  ,input, button, a, img, span)
import Html.Attributes
  exposing (style, align, value, size, href, src, title, alt, width, height)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import Keyboard exposing (KeyCode)

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
pageTitle = KakuroNative.setTitle "Kakuro Dojo"

seedCmd : Cmd Msg
seedCmd =
  Task.perform (\x -> Nop) (\x -> Seed x) Time.now

init : (Model, Cmd Msg)
init = (model, seedCmd)

defaultBoard : IntBoard
defaultBoard =
  Board.make 6 6 0
    |> Board.set 6 7 9
    |> Board.set 1 2 5

model : Model
model =
  let (idx, board) = PuzzleDB.nextBoardOfKind initialKind 0
      state = RenderBoard.makeGameState board
  in
      Model
        initialKind   --kind
        idx           --index
        0             --gencount
        state         --gameState
        Nothing       --seed
        0             --time

-- UPDATE

charToDigit : Int -> Char -> Int
charToDigit default char =
  let res = Char.toCode char - Char.toCode '0'
  in
      if res>=0 && res<=9 then
        res
      else
        default

maybeCharToInt:  Int -> Maybe Char -> Int
maybeCharToInt default mstr =
  case mstr of
      Nothing ->
        default
      Just char ->
        charToDigit default char

updateSelectedCell : String -> Model -> Model
updateSelectedCell idStr model =
  let chars = String.toList idStr
      row = maybeCharToInt -1 <| List.head chars
      col = maybeCharToInt -1 <| List.head <| List.drop 2 chars
  in
      if row >= 0 && col >= 0 then
        let gameState = model.gameState
        in
            { model | gameState =
                { gameState | selection = Just (row, col) }
            }
      else
        model

keyCodeToDigit : Int -> Int -> Int
keyCodeToDigit default keyCode =
  charToDigit default (Char.fromCode keyCode)

type Direction
  = Up
  | Down
  | Right
  | Left

newLocationLoop : Selection -> Selection -> Selection -> Selection -> IntBoard-> Maybe Selection
newLocationLoop min max delta res board =
  let (row, col) = res
      (dr, dc) = delta
      nrow = row + dr
      ncol = col + dc
      nres = (nrow, ncol)
  in
      if nres < min || nres >= max then
        Nothing
      else if (Board.get nrow ncol board) /= 0 then
        Just nres
      else
        newLocationLoop min max delta nres board

newLocation : Selection -> Selection -> IntBoard -> Maybe Selection
newLocation delta selection board =
  let (dr, dc) = delta
      (r, c) = selection
      min = if dr == 0 then (r, 0) else (0, c)
      max = if dr == 0 then (r, board.cols) else (board.rows, c)
  in
      newLocationLoop min max delta selection board

moveSelection : Direction -> Model -> Model
moveSelection direction model =
  let gameState = model.gameState
      board = gameState.board
      rows = board.rows
      cols = board.cols
      selection = case gameState.selection of
                      Nothing -> case direction of
                                     Up ->    (rows, 0)
                                     Down ->  (-1, 0)
                                     Right -> (0, -1)
                                     Left ->  (0, cols)
                      Just sel -> sel
      newSelection = case direction of
                         Up ->    newLocation (-1, 0) selection board
                         Down ->  newLocation (1, 0)  selection board
                         Right -> newLocation (0, 1)  selection board
                         Left ->  newLocation (0, -1) selection board
  in
      case newSelection of
          Nothing -> model
          _ ->
            { model | gameState =
                { gameState | selection = newSelection
                }
            }

movementKeyDirections : List (Char, Direction)
movementKeyDirections = [ ('w', Up)
                        , ('a', Left)
                        , ('s', Down)
                        , ('d', Right)
                        , ('i', Up)
                        , ('j', Left)
                        , ('k', Down)
                        , ('l', Right)
                        , ('&', Up)
                        , ('%', Left)
                        , ('(', Down)
                        , ('\'', Right)
                        ]

-- Process WASD or IJKL.
-- Arrows keys are apparently trapped by the DOM somewhere.
-- Need to figure out how to stop that so we see them.
processMovementKeys : Int -> Model -> Maybe Model
processMovementKeys keyCode model =
  let char = Char.toLower <| Char.fromCode keyCode
  in
      case LE.find (\x -> (fst x) == char) movementKeyDirections of
          Nothing ->
            Nothing
          Just (_, direction) ->
            Just <| moveSelection direction model

processDigitKeys : Int -> Model -> Model
processDigitKeys keyCode model =
  let gameState = model.gameState
      selection = gameState.selection
      guesses = gameState.guesses
  in
      case selection of
          Nothing ->
            model
          Just (row, col) ->
            let char = Char.fromCode keyCode
                digit = charToDigit -1 (if char == ' ' then '0' else char)
            in
                if digit < 0 then
                  model
                else
                  { model | gameState =
                      { gameState | guesses =
                          Board.set row col digit guesses
                      }
                  }
                  
processKeyPress : Int -> Model -> Model
processKeyPress keyCode model =
  case processMovementKeys keyCode model of
      Nothing ->
        processDigitKeys keyCode model
      Just model ->
        model

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    Generate ->
      let (idx, board) = PuzzleDB.nextBoardOfKind model.kind model.index
          gameState = RenderBoard.makeGameState board
      in
          ({model |
             index = idx
           , gencount = (model.gencount+1)
           , gameState = gameState
           },
           Cmd.none)
    Tick time ->
      ({model | time = model.time + 1}, Cmd.none)
    Seed time ->
      ({model | seed = Just <| Random.initialSeed (round time)}, Cmd.none)
    ClickCell id ->
      (updateSelectedCell id model, Cmd.none)
    PressKey code ->
      (processKeyPress code model, Cmd.none)
    Nop ->
      (model, Cmd.none)
          
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  --Time.every second Tick
  Keyboard.downs (PressKey)

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
    , a [ href ("mailto:" ++ email) ]
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
    , div [] [ RenderBoard.render model.gameState ]
    , br
    , div [] [ RenderBoard.renderKeypad ]
    , div []
        [ p []
            [ text "Click to select. Arrows, WASD, or IJKL to move."
            , br
            , text "1-9 to enter number. 0 or space to erase."
            , br
            , text "No validation yet. That's next."
            ]
        , p []
            [ text "Rules: "
            , a [ href "https://en.wikipedia.org/wiki/Kakuro" ]
              [ text "en.wikipedia.org/wiki/Kakuro" ]
            ]
        ]
    , div
        [ id FooterId ]
        [ text (copyright ++ " 2016 Bill St. Clair ")
        , mailLink "billstclair@gmail.com"
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
