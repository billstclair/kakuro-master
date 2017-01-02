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

import SharedTypes exposing ( SavedModel, Model, GameState
                            , Msg, Msg(..), Page(..)
                            , IntBoard, HintsBoard, Selection, Flags
                            , MaybeHelpModelDict(..)
                            )
import Styles.Page exposing (id, class, PId(..), PClass(..))
import Board exposing (Board)
import PuzzleDB
import Entities exposing (nbsp, copyright)
import BoardSize
import DebuggingRender
import RenderBoard
import EncodeDecode exposing ( encodeGameState, encodeSavedModel
                             , decodeGameState, decodeSavedModel)
import HelpBoards exposing ( helpBoards )

import Array exposing (Array)
import Char
import List
import List.Extra as LE
import Dict exposing (Dict)
import String
import Time exposing (Time, second)
import Random
import Task
import Debug exposing (log)
import Html exposing ( Html, Attribute
                     , div, p, h2, h3, text, blockquote
                     , table, tr, td, th
                     , input, button, a, img, span, fieldset, label
                     )
import Html.Attributes exposing ( style, align, value, size
                                , href, target, src, title, alt
                                , width, height
                                , type_, size, placeholder
                                , name, checked
        )
import Html.Events exposing (onClick, onInput)
import Keyboard exposing (KeyCode)
import Window

main : Program (Bool, Maybe String) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }

port setStorage : Maybe String -> Cmd a

port saveGame : (String, String) -> Cmd msg

port requestGame : String -> Cmd msg

port receiveGame : (Maybe String -> msg) -> Sub msg

port setTitle : String -> Cmd msg

port confirmDialog : String -> Cmd msg

port confirmAnswer : ((String, Bool) -> msg) -> Sub msg

-- multiConfirmDialog (message title responses)
-- (Only works in Cordova app)
port multiConfirmDialog : (String, String, List String) -> Cmd msg

-- (message, responseIndex)
port multiConfirmAnswer : ((String, Int) -> msg) -> Sub msg

-- promptDialog (question, default)
port promptDialog : (String, String) -> Cmd msg

-- (question, answer)
port promptAnswer : ((String, String) -> msg) -> Sub msg

-- Copied verbatim from https://github.com/evancz/elm-todomvc/blob/master/Todo.elm

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let ( newModel, cmds ) = update msg model
        savedModel = SharedTypes.modelToSavedModel newModel
        json = encodeSavedModel savedModel
    in
        ( newModel
        , Cmd.batch [ setStorage <| Just json
                    , cmds ]
        )

-- MODEL

initialKind : Int
initialKind =
    6

pageTitle : String
pageTitle =
    "Kakuro Dojo"

seedCmd : Cmd Msg
seedCmd =
    Task.perform (\x -> Seed x) Time.now

init : (Bool, Maybe String) -> ( Model, Cmd Msg )
init state =
    let (isCordova, maybeJson) = state
        savedModel = case maybeJson of
                       Nothing -> Nothing
                       Just json ->
                           case decodeSavedModel json of
                             Err _ -> Nothing
                             Ok savedModel ->
                                 Just savedModel
    in
      ( case savedModel of
          Nothing -> { model | isCordova = isCordova }
          Just m ->
            let res = SharedTypes.savedModelToModel m
            in
                { res
                    | helpModelDict = Javole helpBoards
                    , isCordova = isCordova
                }
      , Cmd.batch
          [ windowSizeCmd
          , setTitle pageTitle
          , seedCmd
          ]
      )

windowSizeCmd : Cmd Msg
windowSizeCmd =
    Task.perform (\x -> WindowSize x) Window.size

model : Model
model =
    let board = PuzzleDB.getBoardOfKind initialKind 1
        state = RenderBoard.makeGameState board
        idx = realBoardIndex board
    in
        { kind = initialKind
        , index = idx
        , indices = [ (6,1), (8,1), (10,1) ]
        , gencount = 0
        , page = HelpPage
        , gameState = state
        , times = SharedTypes.emptyModelTimes
        , windowSize = Nothing
        , boardSizes = Nothing
        , seed = Nothing
        , awaitingCommand = Nothing
        , message = Nothing
        , shifted = False
        , helpModelDict = Javole helpBoards
        , isCordova = False
        }

-- UPDATE

charToDigit : Int -> Char -> Int
charToDigit default char =
    let res = Char.toCode char - Char.toCode '0'
    in
        if res >= 0 && res <= 9 then
            res
        else
            default

toInt : Maybe String -> Int
toInt s =
    case s of
        Nothing -> -1
        Just string ->
            case String.toInt string of
                Ok int -> int
                Err _ -> -1

updateSelection : Model -> Int -> Int -> Model
updateSelection model row col =
    let gameState = model.gameState
    in
        { model
            | gameState =
              { gameState | selection = Just ( row, col ) }
        }
            
updateSelectedCell : String -> Model -> Model
updateSelectedCell idStr model =
    let elts = String.split "," (log "idStr" idStr)
        name = case List.head elts of
                   Nothing -> ""
                   Just n -> n
        row = toInt <| List.head <| List.drop 1 elts
        col = toInt <| List.head <| List.drop 2 elts
    in
        if row >= 0 && col >= 0 then
            if name == "" then
                updateSelection model row col
            else
                case model.helpModelDict of
                    Nicht -> model
                    Javole dict ->
                        case Dict.get name dict of
                            Nothing -> model
                            Just helpModel ->
                                let m = updateSelection helpModel row col
                                    d = Dict.insert name m dict
                                in
                                    { model | helpModelDict = Javole d }
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

newLocationLoop : Selection -> Selection -> Selection -> Selection -> IntBoard -> Maybe Selection
newLocationLoop min max delta res board =
    let ( row, col ) = res
        ( dr, dc ) = delta
        nrow = row + dr
        ncol = col + dc
        nres = ( nrow, ncol )
    in
        if nres < min || nres >= max then
            Nothing
        else if (Board.get nrow ncol board) /= 0 then
            Just nres
        else
            newLocationLoop min max delta nres board

newLocation : Selection -> Selection -> IntBoard -> Maybe Selection
newLocation delta selection board =
    let ( dr, dc ) = delta
        ( r, c ) = selection
        min = if dr == 0 then
                  ( r, 0 )
              else
                  ( 0, c )
        max = if dr == 0 then
                  ( r, board.cols )
              else
                  ( board.rows, c )
    in
        newLocationLoop min max delta selection board

moveSelection : Direction -> Model -> Model
moveSelection direction model =
    let gameState = model.gameState
        board = gameState.board
        rows = board.rows
        cols = board.cols
        selection =
            case gameState.selection of
                Nothing ->
                    case direction of
                        Up -> ( rows, 0 )
                        Down -> ( -1, 0 )
                        Right -> ( 0, -1 )
                        Left -> ( 0, cols )
                Just sel -> sel
        newSelection =
            case direction of
                Up -> newLocation ( -1, 0 ) selection board
                Down -> newLocation ( 1, 0 ) selection board
                Right -> newLocation ( 0, 1 ) selection board
                Left -> newLocation ( 0, -1 ) selection board
    in
        case newSelection of
            Nothing -> model
            _ ->
                { model
                    | gameState =
                        { gameState
                            | selection = newSelection
                        }
                }

movementKeyDirections : List ( Char, Direction )
movementKeyDirections =
    [ ( 'w', Up )
    , ( 'a', Left )
    , ( 's', Down )
    , ( 'd', Right )
    , ( 'i', Up )
    , ( 'j', Left )
    , ( 'k', Down )
    , ( 'l', Right )
    , ( '&', Up )
    , ( '%', Left )
    , ( '(', Down )
    , ( '\'', Right )
    ]

-- Process WASD or IJKL.
-- Arrows keys are apparently trapped by the DOM somewhere.
-- Need to figure out how to stop that so we see them.

processMovementKeys : Int -> Model -> Maybe Model
processMovementKeys keyCode model =
    let char = Char.toLower <| Char.fromCode keyCode
    in
        case LE.find (\x -> (Tuple.first x) == char) movementKeyDirections of
            Nothing -> Nothing
            Just ( _, direction ) ->
                Just <| moveSelection direction model

toggleHint : Int -> Int -> Int -> HintsBoard -> HintsBoard
toggleHint row col digit hints =
    let val = Board.get row col hints
        isThere = List.member digit val
        new =
            if digit == 0 then
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
            Nothing -> model
            Just ( row, col ) ->
                let char = Char.fromCode keyCode
                    digit =
                        charToDigit -1
                            (if char == ' ' then
                                '0'
                             else
                                char
                            )
                in
                    if digit < 0 then
                        model
                    else
                        { model
                            | gameState =
                                if gameState.flags.isHintInput then
                                    { gameState
                                        | hints =
                                            toggleHint row col digit hints
                                    }
                                else
                                    { gameState
                                        | guesses =
                                            Board.set row col digit guesses
                                    }
                        }

shiftKeyCode : Int
shiftKeyCode =
    16

processKeyUp : Int -> Model -> Model
processKeyUp keyCode model =
    if keyCode == shiftKeyCode then
        { model | shifted = False }
    else
        model

processKeyDown : Int -> Model -> Model
processKeyDown keyCode model =
    if keyCode == shiftKeyCode then
        { model | shifted = True }
    else if model.shifted then
        model
    else
        case processMovementKeys keyCode model of
            Nothing -> processDigitKeys keyCode model
            Just model ->
                model

processKeyPress : Int -> Model -> Model
processKeyPress keyCode model =
    let char = Char.fromCode keyCode
    in
        if char == '*' then
            toggleShowPossibilities model
        else if char == '#' then
            toggleHintInput model
        else
            model

realBoardIndex : IntBoard -> Int
realBoardIndex board =
    case board.index of
        Nothing -> 0
        Just index ->
            index

indicesInsert : Int -> Int -> List (Int, Int) -> List (Int, Int)
indicesInsert kind index indices =
    Dict.toList <| Dict.insert kind index <| Dict.fromList indices

newModelIndices : Int -> Model -> List (Int, Int)
newModelIndices newKind model =
    let indices = model.indices
    in
        if newKind == model.kind then
            indices
        else
            indicesInsert model.kind model.index indices

getBoard : Int -> Int -> Model -> ( Model, Cmd a )
getBoard kind index model =
    let index_ =
            if index < 1 then
                PuzzleDB.numberOfBoardsOfKind kind
            else
                index
        board = PuzzleDB.getBoardOfKind kind index_
    in
        case board.spec of
            Nothing ->
                let gameState = RenderBoard.makeGameState board
                    idx = realBoardIndex board
                in
                    ( addBoardSizesToModel
                        { model
                            | gameState = gameState
                            , index = idx
                            , kind = kind
                            , indices = newModelIndices kind model
                        }
                    , Cmd.none
                    )
            Just spec ->
                let currentGameState = model.gameState
                    maybeSpec = currentGameState.board.spec
                in
                    ( { model
                        | awaitingCommand = Just spec
                      }
                    , Cmd.batch
                        (requestGame spec
                            :: case maybeSpec of
                                Nothing -> []
                                Just currentSpec ->
                                    let json = encodeGameState currentGameState
                                    in
                                      [ saveGame ( currentSpec, json ) ]
                        )
                    )

getBoardFromSpec : String -> Model -> Model
getBoardFromSpec spec model =
    let board = PuzzleDB.findBoard spec
        gameState = RenderBoard.makeGameState board
        kind = Board.kind board
        idx = realBoardIndex board
    in
        { model
            | kind = kind
            , index = idx
            , indices = newModelIndices kind model
            , gencount = (model.gencount + 1)
            , gameState = gameState
        }

toggleFlag : (Flags -> Bool) -> (Bool -> Flags -> Flags) -> Model -> Model
toggleFlag reader writer model =
    let gameState = model.gameState
        flags = gameState.flags
        flags_ = writer (not (reader flags)) flags
        gameState_ = { gameState | flags = flags_ }
    in
        { model | gameState = gameState_ }

toggleHintInput : Model -> Model
toggleHintInput model =
    toggleFlag .isHintInput (\v r -> { r | isHintInput = v }) model

toggleShowPossibilities : Model -> Model
toggleShowPossibilities model =
    toggleFlag .showPossibilities (\v r -> { r | showPossibilities = v }) model

resetGameState : Model -> Model
resetGameState model =
    { model | gameState = RenderBoard.makeGameState model.gameState.board }

addBoardSizesToModel : Model -> Model
addBoardSizesToModel model =
    { model | boardSizes = Just <| BoardSize.computeBoardSizes model }

receiveGameJson : Maybe String -> Model -> (Model, Cmd Msg)
receiveGameJson maybeJson model =
    let newBoard =
            (\model2 ->
                 case model.awaitingCommand of
                   Nothing ->
                     ( model2, Cmd.none )
                   Just spec ->
                     ( let model3 = addBoardSizesToModel <|
                                      getBoardFromSpec spec model2
                       in
                         { model3 | awaitingCommand = Nothing }
                     , Cmd.none
                     )
            )
    in                             
      case maybeJson of
        Nothing -> newBoard(model)
        Just json ->
            case decodeGameState json of
              Err _ -> newBoard model
              Ok gameState ->
                  let kind = Board.kind gameState.board
                  in
                      (addBoardSizesToModel
                           { model
                               | gameState = gameState
                               , kind = kind
                               , index = realBoardIndex gameState.board
                               , indices = newModelIndices kind model
                           }
                      , Cmd.none
                      )

timeTick : Time -> Model -> Model
timeTick time model =
    let gameState = model.gameState
        gameStateTimes = gameState.times
        times = model.times
    in
      { model |
        times = { times | timestamp = time }
      , gameState = { gameState |
                      times = { gameStateTimes |
                                elapsed = gameStateTimes.elapsed + 1
                              }
                    }
      }

processWindowSize : Model -> Window.Size -> ( Model, Cmd Msg)
processWindowSize model size =
    ( addBoardSizesToModel { model | windowSize = Just size }
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.page of
        MainPage -> updateMainPage msg model
        _ -> updateHelpPage msg model

updateHelpPage : Msg -> Model -> ( Model, Cmd Msg)
updateHelpPage msg model =
    case msg of
        ShowPage page ->
            ( { model | page = page }
            , Cmd.none
            )
        ClickCell id ->
            ( updateSelectedCell id model, Cmd.none )
        WindowSize size ->
            processWindowSize model size
        _ ->
            ( model
            , Cmd.none )

restartQuery : String
restartQuery =
    "Restart this puzzle?"

processRestartQuery : Bool -> Model -> ( Model, Cmd Msg )
processRestartQuery doit model =
    if doit then
        let gameState = model.gameState
        in
            if (not model.isCordova) &&
               (Board.isBoardEmpty gameState.guesses) &&
               (Board.isBoardEmpty gameState.hints) then
                maybeResetAllGameStates model
            else
                ( resetGameState model, Cmd.none )
    else
        ( model, Cmd.none )

reallyResetAllQuery : String
reallyResetAllQuery =
    "Do you really want to clear all saved games?"

processResetAllQuery : Bool -> Model -> ( Model, Cmd Msg )
processResetAllQuery doit model =
    if doit then
        resetAllGameStates model
    else
        ( model, Cmd.none )

answerProcessors : Dict String (Bool -> Model -> (Model, Cmd Msg))
answerProcessors =
    Dict.fromList
        [ ( restartQuery, processRestartQuery )
        , ( reallyResetAllQuery, processResetAllQuery )
        ]

doAnswerConfirmed : String -> Bool -> Model -> ( Model, Cmd Msg )
doAnswerConfirmed question doit model =
    case Dict.get question answerProcessors of
        Nothing ->
            ( model, Cmd.none )
        Just processor ->
            processor doit model

multiRestartQuery : (String, String, List String)
multiRestartQuery =
    ( restartQuery, "Confirm", [ "Cancel", "Clear All", "OK" ] )

-- TBD
resetAllGameStates : Model -> ( Model, Cmd Msg )
resetAllGameStates oldModel =
    ( { model
      | times = oldModel.times
      , windowSize = oldModel.windowSize
      , seed = oldModel.seed
      , isCordova = oldModel.isCordova
      }
    , setStorage Nothing )

maybeResetAllGameStates : Model -> ( Model, Cmd Msg )
maybeResetAllGameStates model =
    ( model
    , confirmDialog reallyResetAllQuery
    )

processMultiRestartQuery : Int -> Model -> ( Model, Cmd Msg )
processMultiRestartQuery index model =
    case index of
        1 -> maybeResetAllGameStates model
        2 -> processRestartQuery True model
        _ -> ( model, Cmd.none )

multiAnswerProcessors : Dict String (Int -> Model -> (Model, Cmd Msg))
multiAnswerProcessors =
    Dict.fromList
        [ ( restartQuery, processMultiRestartQuery )
        ]

doMultiAnswerConfirmed : String -> Int -> Model -> ( Model, Cmd Msg )
doMultiAnswerConfirmed question index model =
    case Dict.get question multiAnswerProcessors of
        Nothing ->
            ( model, Cmd.none )
        Just processor ->
            processor index model

restartDialog : Model -> Cmd Msg
restartDialog model =
    if model.isCordova then
        multiConfirmDialog multiRestartQuery
    else
        confirmDialog restartQuery

boardIndexQuery : String
boardIndexQuery =
    "Board Index"

processBoardIndexQuery : String -> Model -> ( Model, Cmd Msg )
processBoardIndexQuery idxString model =
    case String.toInt idxString of
        Err _ -> ( model, Cmd.none)
        Ok idx ->
            getBoard model.kind idx model

promptProcessors : Dict String (String -> Model -> ( Model, Cmd Msg ))
promptProcessors =
    Dict.fromList
        [ ( boardIndexQuery, processBoardIndexQuery )
        ]

doPromptAnswerConfirmed : String -> String -> Model -> ( Model, Cmd Msg )
doPromptAnswerConfirmed question answer model =
    case Dict.get question promptProcessors of
        Nothing ->
            ( model, Cmd.none )
        Just processor ->
            processor answer model

getBoardIndex : Model -> ( Model, Cmd Msg )
getBoardIndex model =
    ( model, promptDialog (boardIndexQuery, toString model.index) )

getKindIndex : Int -> Model -> Int
getKindIndex kind model =
    case Dict.get kind (Dict.fromList model.indices) of
        Nothing -> model.index
        Just index -> index

updateMainPage : Msg -> Model -> ( Model, Cmd Msg )
updateMainPage msg model =
    case msg of
        ShowPage page ->
            ( { model | page = page }
            , Cmd.none
            )
        ChangeKind kind ->
            getBoard kind (getKindIndex kind model) model
        Generate increment ->
            getBoard model.kind (model.index + increment) model
        NewBoardIndex indexStr ->
            case String.toInt <| String.right 2 indexStr of
                Err _ -> ( model, Cmd.none )
                Ok index ->
                    getBoard model.kind index model
        Restart ->
            ( model, restartDialog model )
        Tick time ->
            ( timeTick time model, Cmd.none )
        Seed time ->
            ( { model | seed = Just <| Random.initialSeed (round time) }
            , Cmd.none
            )
        ClickCell id ->
            ( updateSelectedCell id model, Cmd.none )
        DownKey code ->
            ( processKeyDown code model, Cmd.none )
        UpKey code ->
            ( processKeyUp code model, Cmd.none )
        PressKey code ->
            ( processKeyPress code model, Cmd.none )
        ToggleHintInput ->
            ( toggleHintInput model, Cmd.none )
        ToggleShowPossibilities ->
            ( toggleShowPossibilities model, Cmd.none )
        ReceiveGame maybeJson ->
            receiveGameJson maybeJson model
        AnswerConfirmed question doit ->
            doAnswerConfirmed question doit model
        MultiAnswerConfirmed question index ->
            doMultiAnswerConfirmed question index model
        PromptAnswerConfirmed question answer ->
            doPromptAnswerConfirmed question answer model
        WindowSize size ->
            processWindowSize model size
        GetBoardIndex ->
            getBoardIndex model
        Nop ->
            ( model, Cmd.none )

-- SUBSCRIPTIONS
-- So far there's only one question, whether to actually clear the board

answerConfirmed : ( String, Bool ) -> Msg
answerConfirmed answer =
    let ( question, doit ) = answer
    in
        AnswerConfirmed question doit

multiAnswerConfirmed : ( String, Int ) -> Msg
multiAnswerConfirmed answer =
    let ( question, index ) = answer
    in
        MultiAnswerConfirmed question index

promptAnswerConfirmed : (String, String) -> Msg
promptAnswerConfirmed result =
    let ( question, answer ) = result
    in
        PromptAnswerConfirmed question answer

subscriptions : Model -> Sub Msg
subscriptions model =
    --Time.every second Tick
    Sub.batch
        [ Keyboard.downs (DownKey)
        , Keyboard.ups (UpKey)
        , Keyboard.presses (PressKey)
        , confirmAnswer answerConfirmed
        , multiConfirmAnswer multiAnswerConfirmed
        , promptAnswer promptAnswerConfirmed
        , receiveGame (\maybeJson -> ReceiveGame maybeJson)
        , Window.resizes (\size -> WindowSize size)
        ]

-- VIEW

sqrimg : String -> String -> Int -> Html Msg
sqrimg url name size =
    img
        [ src url
        , title name
        , alt name
        , width size
        , height size
        ]
        []

logoLink : String -> String -> String -> Int -> Html Msg
logoLink url img name size =
    a [ href url ]
        [ sqrimg ("images/" ++ img) name size ]

showValue : a -> Html Msg
showValue seed =
    div [] [ text <| toString seed ]

br : Html a
br =
    Html.br [] []

mailLink : String -> Html Msg
mailLink email =
    span []
        [ text "<"
        , a [ href ("mailto:" ++ email) ]
            [ text email ]
        , text ">"
        ]

space : Html Msg
space =
    text " "

radio : String -> Bool -> msg -> Html msg
radio value isChecked msg =
    span []
        [ input
            [ type_ "radio"
            , name "board-size"
            , checked isChecked
            , onClick msg
            ]
            []
        , text value
        ]

view : Model -> Html Msg
view model =
    div
        [ align "center"
          --deprecated, so sue me
        ]
        [ Styles.Page.style
        , case model.page of
              MainPage -> mainPageDiv model
              HelpPage -> helpPageDiv model
              TacticsPage -> tacticsPageDiv model
        ]

mainPageDiv : Model -> Html Msg
mainPageDiv model =
    div [ style [ ("margin-top"
                  , (toString <| BoardSize.cordovaTopPad model) ++ "px"
                  )
                ]
        ]
      [ div [ id TopInputId ]
          [ text " "
          , span []
                [ radio "6" (model.kind == 6) (ChangeKind 6)
                , radio "8" (model.kind == 8) (ChangeKind 8)
                , radio "10" (model.kind == 10) (ChangeKind 10)
                ]
          , text " "
          , button
                [ onClick (Generate -1)
                , class ControlsClass
                , title "Go to the previous game."
                ]
                [ text "<" ]
          , text " "
          , button
                [ onClick Restart
                , class ControlsClass
                , title "Start over on this game."
                ]
                [ text "X" ]
          , text " "
          , button
                [ onClick (Generate 1)
                , class ControlsClass
                , title "Go to the next game."
                ]
                [ text ">" ]
          , br
          , a
              [ href "#"
              , onClick <| ShowPage HelpPage
              , title "Show the Help page."
              ]
                [ text <| "Help" ]
          , text " | Board Number: "
          , input [ value <| toString model.index
                  , type_ "number"
                  , size 3
                  , onInput NewBoardIndex
                  , class ControlsClass
                  , style [ ("width", "2em") ]
                  ]
                []
          , text <| case model.message of
                        Nothing -> ""
                        Just hash -> " (" ++ hash ++ ")"
          , br
          -- , text (" " ++ toString model.time)  -- Will eventually be timer
          -- , showValue model.seed               -- debugging
          ]
      , div [] [ RenderBoard.render model ]
      , div [] [ RenderBoard.renderKeypad model ]
      ]

pFormat : String -> List (Html Msg)
pFormat string =
    case String.left 1 string of
        "\t" ->
            [ blockquote [] (pFormat <| String.dropLeft 1 string) ]
        _ ->
            String.split "\n" string
                |> List.map text
                |> List.intersperse br
                |> (\x -> [ p [] x ])

ps : List String -> Html Msg
ps strings =
  List.map pFormat strings
      |> List.concat
      |> div [ class HelpTextClass ]

helpWindowSize : Int -> Int -> Model -> Window.Size
helpWindowSize num denom model =
    let windowSize = case model.windowSize of
                         Nothing -> { width = 256, height = 512 }
                         Just size -> size
        width = min windowSize.width windowSize.height
        size = num * (min width 600) // denom
    in
        { width = size
        , height = 2 * size }

playButton : Html Msg
playButton =
    button
      [ onClick <| ShowPage MainPage
      , class ControlsClass
      , title "Play the game."
      ]
      [ text "Play" ]
    
pageLink : Page -> String -> String -> Html Msg
pageLink page linkText linkTitle =
    a [ href "#"
      , onClick <| ShowPage page
      , title linkTitle
      ]
    [ text linkText ]

renderHelp : String -> Model -> Window.Size -> Html Msg
renderHelp name model size =
    p []
      [
       case model.helpModelDict of
           Nicht ->
             text "No help model dict"
           Javole dict ->
             case Dict.get name dict of
                 Nothing ->
                   text <| "No help model named \"" ++ name ++ "\""
                 Just helpModel ->
                    RenderBoard.renderHelp name helpModel size
      ]

clickTap : Model -> String
clickTap model =
    if model.isCordova then
        "tap"
    else
        "click/tap"

helpPageDiv: Model -> Html Msg
helpPageDiv model =
    let windowSize = helpWindowSize 1 2 model
    in
        div []
            [ h2 [] [ text "Kakuro Dojo" ]
            , div []
                [ p []
                    [ playButton
                    , br , br
                    , pageLink TacticsPage "Tactics" "Show the Tactics page."
                    ]
                , h3 [] [ text "Help" ]
                , ps
                    [ if model.isCordova then
                          "Tap to select.\nArrows to move.\n1-9 to enter number.\n<blank> to erase.\n'*' toggles row/col possibility display.\n'#' toggles hint input."
                      else
                          "Click/Tap to select.\nArrows, WASD, or IJKL to move.\n1-9 to enter number.\n0 or <space> to erase.\n'*' toggles row/col possibility display.\n'#' toggles hint input."
                    ]
                , h3 [] [ text "Rules" ]
                , ps
                     [ "Each contiguous row or column of white squares must contain unique numbers from 1 to 9. The numbers must sum to the number in the gray square to the left of a row or above a column."
                     , "If you repeat a number, or fill a row or column with numbers with an incorrect sum, the possibly wrong numbers will be highlighted in red."
                     , "When you tap '#' to enter hint input mode, you can enter multiple numbers that might be in a square, then use those to eliminate possibilities."
                     , "(The boards below are \"live\". If you " ++ (clickTap model) ++ " a cell, the row and column possibilities will display below the board.)"
                     ]
                , renderHelp "help1" model windowSize
                , renderHelp "help2" model windowSize
                , p []
                    [ text "Also see: "
                    , a [ href "https://en.wikipedia.org/wiki/Kakuro" 
                        , target "_blank"
                        ]
                        [ text "en.wikipedia.org/wiki/Kakuro" ]
                    ]
                , p []
                    [ pageLink TacticsPage "Tactics" "Show the Tactics page."
                    , br , br
                    , playButton
                    ]
                ]
            , footerDiv
            ]

tacticsPageDiv: Model -> Html Msg
tacticsPageDiv model =
    let windowSize = helpWindowSize 1 2 model
        bigWindowSize = helpWindowSize 3 4 model
    in
        div []
            [ div []
                  [ h2 [] [ text "Kakuro Dojo" ]
                  , div []
                      [ p []
                            [ playButton
                            , br , br
                            , pageLink HelpPage "Help" "Show the Help page."
                            ]
                      , h3 [] [ text "Tactics" ]
                      , ps
                           [ "You won't get very far by simply guessing numbers. It helps to use the hints, which are toggled by typing or tapping '#'. You will also learn to recognize clues which have a small number of possible solutions."
                           , "In the following, M numbers that add to N is writtten as N/M. So two numbers that add to 3 is 3/2, and 3 numbers that add to 7 is 7/3. To show the combinations of M numbers that can add to N, start with 'N/M, add an equal sign ('='), then list the combinations. So '3/2 = 12' means that the only combination of 2 numbers that adds to 3 is 1 and 2."
                           , "Here are the common simple sums of two numbers:"
                           , "3/2 = 12\n4/2 = 13\n16/2 = 79\n17/2 = 89"
                           , "The common simple sums of three numbers:"
                           , "6/3 = 123\n7/3 = 124\n23/3 = 689\n24/3 = 789"
                           , "And the common simple sums of four numbers:"
                           , "10/4 = 1234\n11/4 = 1235\n29/4 = 5789\n30/4 = 6789"
                           , "Finally, it's sometimes useful to know the sums of two numbers with two possibilities:"
                           , "5/2 = 14 or 23\n6/2 = 15 or 24\n14/2 = 59 or 68\n15/2 = 69 or 78"
                           ]
                      , p [] [ playButton ]
                      , ps
                           [ "To use this to solve a puzzle, first fill in the possibilities for the simple sums of two or three numbers, intersecting the lists where a row and column cross each other. Here's the example from the 'Help' page, with that done, using the fact, shown in the 'row' possibilities for the 8/3 row that '8/3 = 125 134'."
                           , "(The boards below are \"live\". If you " ++ (clickTap model) ++ " a cell, the row and column possibilities will display below the board.)"
                           ]
                      , renderHelp "help1" model windowSize
                      , ps
                          [ "Next, replace the cells with only one hint number with a real guess, and eliminate that hint number from the other cells in its row and column:"
                          ]
                      , renderHelp "tactics2" model windowSize
                      , ps
                          [ "For this simple example, just iterate until done: "
                          ]
                      , renderHelp "tactics3" model windowSize
                      , renderHelp "tactics4" model windowSize
                      , p [] [ playButton ]
                      , ps
                          [ "A real example has more complicated sums, which you can't fill in right away, but if you use the possibilities display, you can often figure out what to do. For example, below is 6x6 board number 2, with the simple sum hints filled in, and using '15/5 = 12345'."
                          , "There are a few things to notice about that board."
                          , "The right cell of the 11/2 row in the upper-left-hand corner contains only '9', since 11 minus 1 is 10, which doesn't work. This means that the left cell can only contain '2', not '12' as is allowed by 3/2 = 12'. Similarly for the top cell of the 9/2 column in the lower-left-hand corner, and the top cell of the 15/2 column in the lower-right-hand corner."
                          , "The '15/5' column in the middle of the board does not have '12345' as guesses for all of its cells. That's because 4 is not a valid guess for '8/2', being half of 8, the minimum value for '12/2' is 3, and the minimum value for '13/2' is 4. Also the right-most cell of the '15/5' row near the top is missing a 3, since that is half of 6. If you know the numbers that can go in one cell of a 2-cell sum, the other cell's numbers are easy to compute, by subtracting each known number from the sum."
                          ]
                      , renderHelp "board2Model1" model bigWindowSize
                      , ps
                          [ "Filling in the numbers with only one possibility, noticing that 1 is required for the '15/2' column in the center of the board, and it appears only in the '8/2' row, and removing the filled-in numbers from the possibilities in their rows and columns, gives:"
                          ]
                      , renderHelp "board2Model2" model bigWindowSize
                      , ps
                          [ "The left cell of the '8/2' row in the middle of the board is the only cell in its '15/5' column containing a 1, so it must be 1:"
                          ]
                      , renderHelp "board2Model3" model bigWindowSize
                      , ps
                          [ "There is a single blank cell in the '23/5' row near the bottom. We could proceed by noticing that the 2 in the '234' cell to its left is the only 2 in its '15/5' column, but I'm going to talk a little first about how to fill in a single blank cell. Add up the minimum possibilities in its row and column, subtract each from its total, and take the maximum, that's max(23-(1+2+8+3), 34-(5+3+7+8)) = max(9, 11) = 11. Add up the maximum possibilities in its row and column, subtract each from its total, and take the minimum, that's min(23-(1+4+8+3), 34-(6+5+7+9)) = max(7, 7) = 7. So the value in that empty cell needs to be between 7 and 11, i.e. 7, 8, or 9. Since 7 is already in its column, and 8 is in its row, it must be 9."
                          ]
                      , renderHelp "board2Model4" model bigWindowSize
                      , ps
                          [ "The rest can be filled in directly from the possibility row/col display:"
                          ]
                      , renderHelp "board2Model5" model bigWindowSize
                      , p []
                          [ text "Also see: "
                          , a [ href "http://www.kakuro.com/howtoplay.php"
                              , target "_blank"
                              ]
                              [ text "kakuro.com: How to Play" ]
                          ]
                      , p []
                          [ pageLink HelpPage "Help" "Show the Help page."
                          , br , br
                          , playButton
                          ]
                      ]
                  ]
            , footerDiv
            ]

footerDiv : Html Msg
footerDiv =
    div [ id FooterId ]
      [ text "Play online at "
      , a [ href "https://kakuro-dojo.com/" ]
          [ text "kakuro-dojo.com" ]
      , br
      , text (copyright ++ " 2016 Bill St. Clair ")
      , mailLink "billstclair@gmail.com"
      , br
      , logoLink "https://steemit.com/created/kakuro-master"
          "steemit-icon-114x114.png"
          "Steemit articles"
          32
      , space
      , logoLink "https://github.com/billstclair/kakuro-master"
          "GitHub-Mark-32px.png"
          "GitHub source code"
          32
      , space
      , logoLink "http://elm-lang.org/"
          "elm-logo-125x125.png"
          "Elm inside"
          28
      ]
