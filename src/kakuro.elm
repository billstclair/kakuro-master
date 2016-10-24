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

port module Kakuro exposing (..)

import SharedTypes exposing ( SavedModel, modelVersion, Model
                            , GameState
                            , Msg, Msg (..)
                            , IntBoard, HintsBoard, Selection
                            , Flags)
import Styles.Page exposing (id, class, PId(..), PClass(..))
import Board exposing (Board)
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
  ,input, button, a, img, span, fieldset, label)
import Html.Attributes
  exposing (style, align, value, size, href, src, title
           , alt, width, height, type', name, checked)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import Keyboard exposing (KeyCode)
import Window

main : Program (Maybe SavedModel)
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = subscriptions
    }

port setStorage : SavedModel -> Cmd a

port saveGame : (String, GameState) -> Cmd msg

port requestGame : String -> Cmd msg
port receiveGame : (Maybe GameState -> msg) -> Sub msg

port setTitle : String -> Cmd msg

port confirmDialog : String -> Cmd msg
port confirmAnswer : ((String, Bool) -> msg) -> Sub msg

-- Copied verbatim from https://github.com/evancz/elm-todomvc/blob/master/Todo.elm
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
  let
    ( newModel, cmds ) = update msg model
    savedModel = SharedTypes.modelToSavedModel newModel
  in
      ( newModel
      , Cmd.batch [ setStorage savedModel, cmds ]
      )

-- MODEL

initialKind : Int
initialKind = 6

pageTitle : String
pageTitle = "Kakuro Dojo"

seedCmd : Cmd Msg
seedCmd =
  Task.perform (\x -> Nop) (\x -> Seed x) Time.now

init : Maybe SavedModel -> ( Model, Cmd Msg )
init savedModel =
  ( case savedModel of
        Nothing -> model
        Just m -> SharedTypes.savedModelToModel m
  , Cmd.batch [ setTitle pageTitle
              , windowSizeCmd
              , seedCmd
              ]
  )

windowSizeCmd : Cmd Msg
windowSizeCmd =
  Task.perform (\x -> Nop) (\x -> WindowSize x) Window.size

model : Model
model =
  let board = PuzzleDB.getBoardOfKind initialKind 1
      state = RenderBoard.makeGameState board
      idx = realBoardIndex board
  in
      { kind = initialKind
      , index = idx
      , gencount = 0
      , gameState = state
      , time = 0
      , windowSize = Nothing
      , seed = Nothing
      , awaitingCommand = Nothing
      , message = Nothing
      }

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

toggleHint : Int -> Int -> Int -> HintsBoard -> HintsBoard
toggleHint row col digit hints =
  let val = Board.get row col hints
      isThere = List.member digit val
      new = if digit == 0 then
              []
            else if isThere then
              LE.remove digit val
            else
              digit :: val
  in
      Board.set row col new hints

processDigitKeys : Int -> Model -> Model
processDigitKeys keyCode model =
  let gameState = model.gameState
      selection = gameState.selection
      guesses = gameState.guesses
      hints = gameState.hints
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
                      if gameState.flags.isHintInput then
                        { gameState | hints =
                            toggleHint row col digit hints
                        }
                      else
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

realBoardIndex : IntBoard -> Int
realBoardIndex board =
  case board.index of
      Nothing -> 0
      Just index -> index

getBoard : Int -> Int -> Model -> (Model, Cmd a)
getBoard kind index model =
  let index' = if index < 1 then
                 PuzzleDB.numberOfBoardsOfKind kind
               else
                 index
      board = PuzzleDB.getBoardOfKind kind index'
  in
      case board.spec of
          Nothing ->
            let gameState = RenderBoard.makeGameState board
                idx = realBoardIndex board
            in
                ( { model |
                    gameState = gameState
                  , index = idx
                  , kind = kind
                  }
                , Cmd.none)
          Just spec ->
            let currentGameState = model.gameState
                maybeSpec = currentGameState.board.spec
            in
                ( { model |
                      awaitingCommand = Just spec
                  }
                , Cmd.batch ( requestGame spec
                            ::
                              case maybeSpec of
                                  Nothing -> []
                                  Just currentSpec ->
                                    [ saveGame (currentSpec, currentGameState) ]
                            )
                )

getBoardFromSpec : String -> Model -> Model
getBoardFromSpec spec model =
  let board = PuzzleDB.findBoard spec
      gameState = RenderBoard.makeGameState board
      idx = realBoardIndex board
      in
          { model |
            kind = Board.kind board
          , index = idx
          , gencount = (model.gencount+1)
          , gameState = gameState
          }

toggleFlag : ( Flags -> Bool) -> ( Bool -> Flags -> Flags) -> Model -> Model
toggleFlag reader writer model =
  let gameState = model.gameState
      flags = gameState.flags
      flags' = writer (not (reader flags)) flags
      gameState' = { gameState | flags = flags' }
  in
      { model | gameState = gameState' }

toggleHintInput : Model -> Model
toggleHintInput model =
  toggleFlag .isHintInput (\v r -> { r | isHintInput = v }) model

toggleShowPossibilities : Model -> Model
toggleShowPossibilities model =
  toggleFlag .showPossibilities (\v r -> { r | showPossibilities = v }) model

resetGameState : Model -> Model
resetGameState model =
  { model | gameState = RenderBoard.makeGameState model.gameState.board }

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    ChangeKind kind ->
      getBoard kind model.index model
    Generate increment ->
      getBoard model.kind (model.index + increment) model
    Restart ->
      (model, confirmDialog "Restart this puzzle?")
    Tick time ->
      ({ model | time = model.time + 1 }, Cmd.none)
    Seed time ->
      ( { model | seed = Just <| Random.initialSeed (round time) }
      , Cmd.none)
    ClickCell id ->
      (updateSelectedCell id model, Cmd.none)
    PressKey code ->
      (processKeyPress code model, Cmd.none)
    ToggleHintInput ->
      (toggleHintInput model, Cmd.none)
    ToggleShowPossibilities ->
      (toggleShowPossibilities model, Cmd.none)
    ReceiveGame maybeGameState ->
      case maybeGameState of
          Nothing ->
            case model.awaitingCommand of
                Nothing ->
                  (model, Cmd.none)
                Just spec ->
                  (getBoardFromSpec spec model, Cmd.none)
          Just gameState ->
            ( { model |
                gameState = gameState
              , kind = Board.kind gameState.board
              , index = realBoardIndex gameState.board
              }
            , Cmd.none
            )
    AnswerConfirmed question doit ->
      ( if doit then (resetGameState model) else model
      , Cmd.none)
    WindowSize size ->
      ( { model | windowSize = Just size }, Cmd.none)
    Nop ->
      (model, Cmd.none)
          
-- SUBSCRIPTIONS

-- So far there's only one question, whether to actually clear the board
answerConfirmed : (String, Bool) -> Msg
answerConfirmed answer =
  let (question, doit) = answer
  in
      AnswerConfirmed question doit

subscriptions : Model -> Sub Msg
subscriptions model =
  --Time.every second Tick
  Sub.batch [ Keyboard.downs (PressKey)
            , confirmAnswer answerConfirmed
            , receiveGame (\maybeGame -> ReceiveGame maybeGame)
            , Window.resizes (\size -> WindowSize size)
            ]

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

radio : String -> Bool -> msg -> Html msg
radio value isChecked msg =
  span []
    [ input [ type' "radio"
            , name "board-size"
            , checked isChecked
            , onClick msg ] []
    , text value
    ]

view : Model -> Html Msg
view model =
  div [ align "center" --deprecated, so sue me
      ]
    [ Styles.Page.style
    , h2 [] [text pageTitle]
    , div
        [ id TopInputId ]
        [ span []
            [ radio "6" (model.kind == 6) (ChangeKind 6)
            , radio "8" (model.kind == 8) (ChangeKind 8)
            , radio "10" (model.kind == 10) (ChangeKind 10)
            ]
        , text " "
        , button [ onClick (Generate -1)
                 , class ControlsClass
                 , title "Go to the previous game." ]
           [ text "<" ]
        , text " "
        , button [ onClick Restart
                 , class ControlsClass
                 , title "Start over on this game." ]
          [ text "X" ]
        , text " "
        , button [ onClick (Generate 1)
                 , class ControlsClass
                 , title "Go to the next game." ]
           [ text ">" ]
        , br
        , text "Board Number: "
        , text ((toString model.index) ++
          case model.message of
              Nothing -> ""
              Just hash ->
                " (" ++ hash ++ ")"
               )
        , br
        -- , text (" " ++ toString model.time)  -- Will eventually be timer
        -- , showValue model.seed               -- debugging
        ]
    , div [] [ RenderBoard.render model ]
    , div [] [ RenderBoard.renderKeypad model.gameState ]
    , div []
        [ p []
            [ text "Click to select. Arrows, WASD, or IJKL to move."
            , br
            , text "1-9 to enter number. 0 to erase."
            , br
            , text "* toggles row/col possibility display."
            , br
            , text "# toggles hint input."
            , br
            , text "Under development. New features daily."
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
