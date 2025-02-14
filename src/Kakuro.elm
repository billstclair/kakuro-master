---------------------------------------------------------------------
--
-- Kakuro.elm
-- kakuro-dojo.com main screen
-- Copyright (c) 2016-2025 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------
{-

   TODO:

   Generate.elm

-}


port module Kakuro exposing (main)

import Array exposing (Array)
import Board exposing (Board)
import BoardSize
import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Char
import Date
import Debug exposing (log)
import DebuggingRender
import Dict exposing (Dict)
import EncodeDecode
    exposing
        ( decodeGameState
        , decodeSavedModel
        , encodeGameState
        , encodeSavedModel
        )
import Entities exposing (copyright, nbsp)
import HelpBoards exposing (helpBoards)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , blockquote
        , button
        , div
        , fieldset
        , h2
        , h3
        , h4
        , img
        , input
        , label
        , p
        , span
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
        , checked
        , colspan
        , disabled
        , height
        , href
        , name
        , placeholder
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List
import List.Extra as LE
import ModalDialog exposing (modalDiv)
import PuzzleDB
import Random
import RenderBoard
import SharedTypes
    exposing
        ( ExploreState
        , Flags
        , GameState
        , HintsBoard
        , IntBoard
        , MaybeHelpModelDict(..)
        , Model
        , Msg(..)
        , Page(..)
        , Platform(..)
        , SavedModel
        , Selection
        , WindowSize
        )
import String
import Styles.Page exposing (PClass(..), PId(..), class, id)
import Task
import Time exposing (Posix)


main : Program ( String, List ( String, String ), Maybe String ) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


port setStorage : Maybe String -> Cmd a


port saveGame : ( String, String ) -> Cmd msg


port requestGame : String -> Cmd msg


port receiveGame : (Maybe String -> msg) -> Sub msg



-- getProperty is intentionally missing.
-- The properties are all cached in the Model.
-- Writes have to go to both, but reads can come from the Model.


port setTitle : String -> Cmd msg


port confirmDialog : String -> Cmd msg


port confirmAnswer : (( String, Bool ) -> msg) -> Sub msg



-- multiConfirmDialog (message title responses)
-- (Only works in Cordova app)


port multiConfirmDialog : ( String, String, List String ) -> Cmd msg



-- (message, responseIndex)


port multiConfirmAnswer : (( String, Int ) -> msg) -> Sub msg



-- promptDialog (question, default)


port promptDialog : ( String, String ) -> Cmd msg



-- (question, answer)


port promptAnswer : (( String, String ) -> msg) -> Sub msg


port makeClickSound : () -> Cmd msg


port deviceReady : (String -> msg) -> Sub msg



-- Copied verbatim from https://github.com/evancz/elm-todomvc/blob/master/Todo.elm


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update (Debug.log "updateWithStorage, msg" msg) model

        savedModel =
            SharedTypes.modelToSavedModel newModel

        doWrite =
            case newModel.savedModel of
                Nothing ->
                    True

                Just sm ->
                    savedModel /= sm
    in
    if not doWrite then
        ( newModel, cmds )

    else
        let
            json =
                encodeSavedModel savedModel
        in
        ( { newModel | savedModel = Just savedModel }
        , Cmd.batch
            [ setStorage <| Just <| JE.encode 0 json
            , cmds
            ]
        )


maybeMakeClickSound : Model -> Cmd Msg
maybeMakeClickSound model =
    if model.gameState.flags.keyClickSound then
        makeClickSound ()

    else
        Cmd.none



-- MODEL


initialKind : Int
initialKind =
    6


pageTitle : String
pageTitle =
    "Kakuro Dojo"


seedCmd : Cmd Msg
seedCmd =
    Task.perform (\posix -> Seed posix) Time.now


decodePlatform : String -> Platform
decodePlatform name =
    case name of
        "iOS" ->
            IosPlatform

        "Android" ->
            AndroidPlatform

        _ ->
            WebPlatform


init : ( String, List ( String, String ), Maybe String ) -> ( Model, Cmd Msg )
init state =
    let
        ( platformName, properties, maybeJson ) =
            state

        platform =
            decodePlatform platformName

        savedModel =
            case maybeJson of
                Nothing ->
                    Nothing

                Just json ->
                    case decodeSavedModel json of
                        Err _ ->
                            Nothing

                        Ok sm ->
                            Just sm

        propertiesDict =
            Dict.fromList properties

        mod =
            case savedModel of
                Nothing ->
                    { initialModel
                        | platform = platform
                        , properties = propertiesDict
                    }

                Just m ->
                    let
                        res =
                            SharedTypes.savedModelToModel m
                    in
                    { res
                        | helpModelDict = Javole helpBoards
                        , platform = platform
                        , properties = propertiesDict
                    }
    in
    ( mod
    , Cmd.batch
        [ Task.perform SetWindowSize Dom.getViewport
        , setTitle pageTitle
        , seedCmd
        ]
    )


initialModel : Model
initialModel =
    let
        board =
            PuzzleDB.getBoardOfKind initialKind 1

        state =
            RenderBoard.makeGameState board

        idx =
            realBoardIndex board
    in
    { kind = initialKind
    , index = idx
    , indices = [ ( 6, 1 ), ( 8, 1 ), ( 10, 1 ) ]
    , gencount = 0
    , page = HelpPage
    , gameState = state
    , windowSize = Nothing
    , boardSizes = Nothing
    , seed = Nothing
    , awaitingCommand = Nothing
    , message = Nothing
    , shifted = False
    , showStarMenu = False
    , helpModelDict = Javole helpBoards
    , platform = WebPlatform
    , properties = Dict.fromList []
    , deviceReady = False
    , savedModel = Nothing
    }



-- UPDATE


stringToDigit : Int -> String -> Int
stringToDigit default string =
    case String.toInt string of
        Nothing ->
            default

        Just res ->
            if res >= 0 && res <= 9 then
                res

            else
                default


toInt : Maybe String -> Int
toInt s =
    case s of
        Nothing ->
            -1

        Just string ->
            case String.toInt string of
                Just int ->
                    int

                Nothing ->
                    -1


updateSelection : Model -> Int -> Int -> Model
updateSelection model row col =
    let
        gameState =
            model.gameState

        flags =
            gameState.flags
    in
    { model
        | gameState =
            { gameState
                | selection = Just ( row, col )
                , flags = { flags | firstGuess = 0 }
            }
        , message = Nothing
    }


updateSelectedCell : String -> Model -> Model
updateSelectedCell idStr model =
    let
        elts =
            String.split "," idStr

        name =
            case List.head elts of
                Nothing ->
                    ""

                Just n ->
                    n

        row =
            toInt <| List.head <| List.drop 1 elts

        col =
            toInt <| List.head <| List.drop 2 elts
    in
    if row >= 0 && col >= 0 then
        if name == "" then
            updateSelection model row col

        else
            case model.helpModelDict of
                Nicht ->
                    model

                Javole dict ->
                    case Dict.get name dict of
                        Nothing ->
                            model

                        Just helpModel ->
                            let
                                m =
                                    updateSelection helpModel row col

                                d =
                                    Dict.insert name m dict
                            in
                            { model | helpModelDict = Javole d }

    else
        model


type Direction
    = Up
    | Down
    | Right
    | Left


newLocationLoop : Selection -> Selection -> Selection -> Selection -> IntBoard -> Maybe Selection
newLocationLoop min max delta res board =
    let
        ( row, col ) =
            res

        ( dr, dc ) =
            delta

        nrow =
            row + dr

        ncol =
            col + dc

        nres =
            ( nrow, ncol )
    in
    if nres < min || nres >= max then
        Nothing

    else if Board.get nrow ncol board /= 0 then
        Just nres

    else
        newLocationLoop min max delta nres board


newLocation : Selection -> Selection -> IntBoard -> Maybe Selection
newLocation delta selection board =
    let
        ( dr, dc ) =
            delta

        ( r, c ) =
            selection

        min =
            if dr == 0 then
                ( r, 0 )

            else
                ( 0, c )

        max =
            if dr == 0 then
                ( r, board.cols )

            else
                ( board.rows, c )
    in
    newLocationLoop min max delta selection board


moveSelection : Direction -> Model -> Model
moveSelection direction model =
    let
        gameState =
            model.gameState

        board =
            gameState.board

        rows =
            board.rows

        cols =
            board.cols

        selection =
            case gameState.selection of
                Nothing ->
                    case direction of
                        Up ->
                            ( rows, 0 )

                        Down ->
                            ( -1, 0 )

                        Right ->
                            ( 0, -1 )

                        Left ->
                            ( 0, cols )

                Just sel ->
                    sel

        newSelection =
            case direction of
                Up ->
                    newLocation ( -1, 0 ) selection board

                Down ->
                    newLocation ( 1, 0 ) selection board

                Right ->
                    newLocation ( 0, 1 ) selection board

                Left ->
                    newLocation ( 0, -1 ) selection board

        flags =
            gameState.flags
    in
    case newSelection of
        Nothing ->
            model

        _ ->
            { model
                | gameState =
                    { gameState
                        | selection = newSelection
                        , flags = { flags | firstGuess = 0 }
                    }
                , message = Nothing
            }


movementKeyDirections : List ( String, Direction )
movementKeyDirections =
    [ ( "w", Up )
    , ( "a", Left )
    , ( "s", Down )
    , ( "d", Right )
    , ( "i", Up )
    , ( "j", Left )
    , ( "k", Down )
    , ( "l", Right )
    , ( "arrowup", Up )
    , ( "arrowleft", Left )
    , ( "arrowdown", Down )
    , ( "arrowright", Right )
    ]



-- Process WASD or IJKL.
-- Arrows keys are apparently trapped by the DOM somewhere.
-- Need to figure out how to stop that so we see them.


processMovementKeys : String -> Model -> Maybe Model
processMovementKeys key model =
    let
        char =
            String.toLower key
    in
    case LE.find (\x -> Tuple.first x == char) movementKeyDirections of
        Nothing ->
            Nothing

        Just ( _, direction ) ->
            Just <| moveSelection direction model


toggleHint : Int -> Int -> Int -> HintsBoard -> HintsBoard
toggleHint row col digit hints =
    let
        val =
            Board.get row col hints

        isThere =
            List.member digit val

        new =
            if digit == 0 then
                []

            else if isThere then
                LE.remove digit val

            else
                digit :: val
    in
    Board.set row col new hints


updateExploreState : GameState -> Int -> Int -> Int -> Maybe ExploreState
updateExploreState gameState row col digit =
    case gameState.exploreState of
        Nothing ->
            Nothing

        Just state ->
            let
                st2 =
                    { state
                        | guesses = Board.set row col digit state.guesses
                    }

                st3 =
                    if st2.firstGuess /= 0 then
                        st2

                    else
                        { st2
                            | firstGuess = digit
                            , firstGuessSelection = gameState.selection
                        }
            in
            Just st3


processDigitKeys : String -> Model -> Model
processDigitKeys key model =
    let
        gameState =
            model.gameState

        selection =
            gameState.selection

        guesses =
            gameState.guesses

        hints =
            gameState.hints

        flags =
            gameState.flags
    in
    case selection of
        Nothing ->
            model

        Just ( row, col ) ->
            let
                digit =
                    stringToDigit -1
                        (if key == " " then
                            "0"

                         else
                            key
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
                                , flags = { flags | firstGuess = 0 }
                                , exploreState =
                                    updateExploreState
                                        gameState
                                        row
                                        col
                                        digit
                            }
                }


shiftKey : String
shiftKey =
    "Shift"


processKeyUp : String -> Model -> Model
processKeyUp key model =
    if key == shiftKey then
        { model | shifted = False }

    else
        model


processKeyDown : String -> Model -> Model
processKeyDown key model =
    if key == shiftKey then
        { model | shifted = True }

    else if model.shifted then
        model

    else
        case processMovementKeys key model of
            Nothing ->
                processDigitKeys key model

            Just mdl ->
                mdl


processKeyPress : String -> Model -> Model
processKeyPress key model =
    if key == "*" then
        { model | showStarMenu = True }

    else if key == "#" then
        toggleHintInput model

    else
        model


realBoardIndex : IntBoard -> Int
realBoardIndex board =
    case board.index of
        Nothing ->
            0

        Just index ->
            index


indicesInsert : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
indicesInsert kind index indices =
    Dict.toList <| Dict.insert kind index <| Dict.fromList indices


newModelIndices : Int -> Model -> List ( Int, Int )
newModelIndices newKind model =
    let
        indices =
            model.indices
    in
    if newKind == model.kind then
        indices

    else
        indicesInsert model.kind model.index indices


getBoard : Int -> Int -> Model -> ( Model, Cmd a )
getBoard kind index model =
    let
        index_ =
            if index < 1 then
                PuzzleDB.numberOfBoardsOfKind kind

            else
                index

        board =
            PuzzleDB.getBoardOfKind kind index_
    in
    gotoBoard board model


gotoBoard : IntBoard -> Model -> ( Model, Cmd a )
gotoBoard board model =
    case board.spec of
        Nothing ->
            let
                gameState =
                    RenderBoard.makeGameState board

                idx =
                    realBoardIndex board

                kind =
                    board.rows
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
            let
                currentGameState =
                    model.gameState

                maybeSpec =
                    currentGameState.board.spec
            in
            ( { model
                | awaitingCommand = Just spec
              }
            , Cmd.batch
                (requestGame spec
                    :: (case maybeSpec of
                            Nothing ->
                                []

                            Just currentSpec ->
                                let
                                    json =
                                        encodeGameState currentGameState
                                in
                                [ saveGame ( currentSpec, JE.encode 0 json ) ]
                       )
                )
            )


getBoardFromSpec : String -> Model -> Model
getBoardFromSpec spec model =
    let
        board =
            PuzzleDB.findBoard spec

        gameState =
            RenderBoard.makeGameState board

        kind =
            Board.kind board

        idx =
            realBoardIndex board
    in
    { model
        | kind = kind
        , index = idx
        , indices = newModelIndices kind model
        , gencount = model.gencount + 1
        , gameState = gameState
    }


toggleFlag : (Flags -> Bool) -> (Bool -> Flags -> Flags) -> Model -> Model
toggleFlag reader writer model =
    let
        gameState =
            model.gameState

        flags =
            gameState.flags

        flags_ =
            writer (not (reader flags)) flags

        gameState_ =
            { gameState | flags = flags_ }
    in
    { model | gameState = gameState_ }


toggleHintInput : Model -> Model
toggleHintInput model =
    toggleFlag .isHintInput (\v r -> { r | isHintInput = v }) model


toggleShowPossibilities : Model -> Model
toggleShowPossibilities model =
    let
        m =
            toggleFlag
                .showPossibilities
                (\v r -> { r | showPossibilities = v })
                model
    in
    { m | showStarMenu = False }


toggleKeyClick : Model -> Model
toggleKeyClick model =
    let
        m =
            toggleFlag
                .keyClickSound
                (\v r -> { r | keyClickSound = v })
                model
    in
    { m | showStarMenu = False }


startExploration : Model -> Model
startExploration model =
    let
        gameState =
            model.gameState

        guesses =
            gameState.guesses

        kind =
            guesses.rows

        exploreState =
            { savedBoard = guesses
            , savedHints = gameState.hints
            , guesses = Board.make kind kind 0
            , firstGuess = 0
            , firstGuessSelection = gameState.selection
            }

        newState =
            { gameState | exploreState = Just exploreState }
    in
    { model
        | gameState = newState
        , showStarMenu = False
    }


keepExploration : Model -> Model
keepExploration model =
    let
        gameState =
            model.gameState

        flags =
            gameState.flags
    in
    { model
        | gameState =
            { gameState
                | exploreState = Nothing
                , flags = { flags | firstGuess = 0 }
            }
        , showStarMenu = False
    }


discardExploration : Model -> Model
discardExploration model =
    let
        gameState =
            model.gameState
    in
    case gameState.exploreState of
        Nothing ->
            model

        Just state ->
            let
                selection =
                    case state.firstGuessSelection of
                        Nothing ->
                            gameState.selection

                        Just sel ->
                            Just sel

                flags =
                    gameState.flags

                newState =
                    { gameState
                        | guesses = state.savedBoard
                        , hints = state.savedHints
                        , flags =
                            { flags
                                | firstGuess = state.firstGuess
                            }
                        , selection = selection
                        , exploreState = Nothing
                    }
            in
            { model
                | gameState = newState
                , showStarMenu = False
            }


resetGameState : Model -> Model
resetGameState model =
    { model | gameState = RenderBoard.makeGameState model.gameState.board }


addBoardSizesToModel : Model -> Model
addBoardSizesToModel model =
    { model | boardSizes = Just <| BoardSize.computeBoardSizes model }


receiveGameJson : Maybe String -> Model -> ( Model, Cmd Msg )
receiveGameJson maybeJson model =
    let
        newBoard =
            \model2 ->
                case model.awaitingCommand of
                    Nothing ->
                        ( model2, Cmd.none )

                    Just spec ->
                        ( let
                            model3 =
                                addBoardSizesToModel <|
                                    getBoardFromSpec spec model2
                          in
                          { model3 | awaitingCommand = Nothing }
                        , Cmd.none
                        )
    in
    case maybeJson of
        Nothing ->
            newBoard model

        Just json ->
            case decodeGameState json of
                Err _ ->
                    newBoard model

                Ok gameState ->
                    let
                        kind =
                            Board.kind gameState.board
                    in
                    ( addBoardSizesToModel
                        { model
                            | gameState = gameState
                            , kind = kind
                            , index = realBoardIndex gameState.board
                            , indices = newModelIndices kind model
                        }
                    , Cmd.none
                    )


formatPosix : String -> Posix -> String
formatPosix format posix =
    Date.format format <| Date.fromPosix Time.utc posix


updateWindowSize : Int -> Int -> Model -> ( Model, Cmd Msg )
updateWindowSize w h model =
    ( addBoardSizesToModel
        { model
            | windowSize =
                Just { width = w, height = h }
        }
    , Cmd.none
    )


processWindowSize : Model -> Viewport -> ( Model, Cmd Msg )
processWindowSize model viewport =
    let
        vp =
            viewport.viewport
    in
    updateWindowSize (round vp.width) (round vp.height) model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.page of
        MainPage ->
            updateMainPage msg model

        _ ->
            updateHelpPage msg model


gotDeviceReady : Model -> String -> ( Model, Cmd Msg )
gotDeviceReady model platformName =
    let
        platform =
            decodePlatform platformName

        m =
            { model
                | deviceReady = True
                , platform = platform
            }

        m2 =
            addBoardSizesToModel m
    in
    ( m2, Cmd.none )


extraPuzzlesProductId : String
extraPuzzlesProductId =
    "puzzles2"


updateHelpPage : Msg -> Model -> ( Model, Cmd Msg )
updateHelpPage msg model =
    case msg of
        ShowPage page ->
            ( { model | page = page, message = Nothing }
            , Cmd.none
            )

        ReceiveGame maybeJson ->
            receiveGameJson maybeJson model

        Seed posix ->
            doSeed posix model

        ClickCell id ->
            ( updateSelectedCell id model, maybeMakeClickSound model )

        SetWindowSize viewport ->
            processWindowSize model viewport

        UpdateWindowSize w h ->
            updateWindowSize w h model

        DeviceReady platformName ->
            gotDeviceReady model platformName

        _ ->
            ( model
            , Cmd.none
            )


restartQuery : String
restartQuery =
    "Restart this puzzle?"


resetAllQuery : String
resetAllQuery =
    "Clear all saved puzzles?"


processRestartQuery : Bool -> Model -> ( Model, Cmd Msg )
processRestartQuery doit model =
    if doit then
        let
            gameState =
                model.gameState
        in
        if
            (model.platform == WebPlatform)
                && Board.isBoardEmpty gameState.guesses
                && Board.isBoardEmpty gameState.hints
        then
            maybeResetAllGameStates model

        else
            ( resetGameState model, Cmd.none )

    else
        ( model, Cmd.none )


reallyResetAllQuery : String
reallyResetAllQuery =
    "Are you sure you want to clear all saved puzzles?"


processResetAllQuery : Bool -> Model -> ( Model, Cmd Msg )
processResetAllQuery doit model =
    if doit then
        resetAllGameStates model

    else
        ( model, Cmd.none )


answerProcessors : Dict String (Bool -> Model -> ( Model, Cmd Msg ))
answerProcessors =
    Dict.fromList
        [ ( restartQuery, processRestartQuery )
        , ( reallyResetAllQuery, processResetAllQuery )
        , ( resetAllQuery, processRestartQuery )
        ]


doAnswerConfirmed : String -> Bool -> Model -> ( Model, Cmd Msg )
doAnswerConfirmed question doit model =
    case Dict.get question answerProcessors of
        Nothing ->
            ( model, Cmd.none )

        Just processor ->
            processor doit model


multiRestartQuery : ( String, String, List String )
multiRestartQuery =
    ( restartQuery, "Confirm", [ "Cancel", "Clear All Saved Puzzles", "Restart This Puzzle" ] )



-- TBD


resetAllGameStates : Model -> ( Model, Cmd Msg )
resetAllGameStates oldModel =
    let
        mdl =
            { initialModel
                | windowSize = oldModel.windowSize
                , seed = oldModel.seed
                , platform = oldModel.platform
            }
    in
    ( { mdl | savedModel = Just <| SharedTypes.modelToSavedModel mdl }
    , setStorage Nothing
    )


maybeResetAllGameStates : Model -> ( Model, Cmd Msg )
maybeResetAllGameStates model =
    ( model
    , confirmDialog reallyResetAllQuery
    )


processMultiRestartQuery : Int -> Model -> ( Model, Cmd Msg )
processMultiRestartQuery index model =
    case index of
        1 ->
            maybeResetAllGameStates model

        2 ->
            processRestartQuery True model

        _ ->
            ( model, Cmd.none )


multiAnswerProcessors : Dict String (Int -> Model -> ( Model, Cmd Msg ))
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
    case model.platform of
        WebPlatform ->
            let
                gameState =
                    model.gameState

                query =
                    if
                        Board.isBoardEmpty gameState.guesses
                            && Board.isBoardEmpty gameState.hints
                    then
                        resetAllQuery

                    else
                        restartQuery
            in
            confirmDialog query

        _ ->
            multiConfirmDialog multiRestartQuery


boardIndexQuery : String
boardIndexQuery =
    "Board Index"


processBoardIndexQuery : String -> Model -> ( Model, Cmd Msg )
processBoardIndexQuery idxString model =
    case String.toInt idxString of
        Nothing ->
            ( model, Cmd.none )

        Just idx ->
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
    ( model, promptDialog ( boardIndexQuery, String.fromInt model.index ) )


getKindIndex : Int -> Model -> Int
getKindIndex kind model =
    case Dict.get kind (Dict.fromList model.indices) of
        Nothing ->
            model.index

        Just index ->
            index


enableAllBoardsWithoutPurchase : Bool
enableAllBoardsWithoutPurchase =
    True


restrictBoards : Model -> Bool
restrictBoards model =
    False


doSeed : Posix -> Model -> ( Model, Cmd Msg )
doSeed posix model =
    let
        m =
            { model
                | seed =
                    Just <| Random.initialSeed (Time.posixToMillis posix)
            }
    in
    ( m, Cmd.none )


processNewBoardIndex : String -> Model -> ( Model, Cmd Msg )
processNewBoardIndex indexStr model =
    case String.toInt <| String.right 2 indexStr of
        Nothing ->
            ( model, Cmd.none )

        Just index ->
            let
                maxidx =
                    PuzzleDB.numberOfBoardsOfKind model.kind

                idx =
                    if index >= maxidx then
                        case String.toInt <| String.right 1 indexStr of
                            Nothing ->
                                index

                            Just i ->
                                if i < 1 then
                                    1

                                else
                                    i

                    else
                        index
            in
            getBoard model.kind idx model


updateMainPage : Msg -> Model -> ( Model, Cmd Msg )
updateMainPage msg model =
    case msg of
        ShowPage page ->
            ( { model | page = page, message = Nothing }
            , Cmd.none
            )

        ChangeKind kind ->
            getBoard kind (getKindIndex kind model) model

        Generate increment ->
            getBoard model.kind (model.index + increment) model

        NewBoardIndex indexStr ->
            processNewBoardIndex indexStr model

        Restart ->
            ( model, restartDialog model )

        Seed time ->
            doSeed time model

        ClickCell id ->
            ( updateSelectedCell id model, maybeMakeClickSound model )

        DownKey makeClick code ->
            ( processKeyDown code model
            , if makeClick then
                maybeMakeClickSound model

              else
                Cmd.none
            )

        UpKey code ->
            ( processKeyUp code model, Cmd.none )

        PressKey code ->
            ( processKeyPress code model, Cmd.none )

        ToggleHintInput ->
            ( toggleHintInput model, maybeMakeClickSound model )

        ToggleShowPossibilities ->
            ( toggleShowPossibilities model, Cmd.none )

        ToggleKeyClick ->
            ( toggleKeyClick model, Cmd.none )

        OpenStarMenu ->
            ( { model | showStarMenu = True }, maybeMakeClickSound model )

        StartExploration ->
            ( startExploration model, Cmd.none )

        KeepExploration ->
            ( keepExploration model, Cmd.none )

        DiscardExploration ->
            ( discardExploration model, Cmd.none )

        CloseStarMenu ->
            ( { model | showStarMenu = False }, Cmd.none )

        ReceiveGame maybeJson ->
            receiveGameJson maybeJson model

        AnswerConfirmed question doit ->
            doAnswerConfirmed question doit model

        MultiAnswerConfirmed question index ->
            doMultiAnswerConfirmed question index model

        PromptAnswerConfirmed question answer ->
            doPromptAnswerConfirmed question answer model

        DeviceReady platformName ->
            gotDeviceReady model platformName

        SetWindowSize viewport ->
            processWindowSize model viewport

        UpdateWindowSize w h ->
            updateWindowSize w h model

        GetBoardIndex ->
            getBoardIndex model

        Nop ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS
-- So far there's only one question, whether to actually clear the board


answerConfirmed : ( String, Bool ) -> Msg
answerConfirmed answer =
    let
        ( question, doit ) =
            answer
    in
    AnswerConfirmed question doit


multiAnswerConfirmed : ( String, Int ) -> Msg
multiAnswerConfirmed answer =
    let
        ( question, index ) =
            answer
    in
    MultiAnswerConfirmed question index


promptAnswerConfirmed : ( String, String ) -> Msg
promptAnswerConfirmed result =
    let
        ( question, answer ) =
            result
    in
    PromptAnswerConfirmed question answer


keyDecoder : (String -> Msg) -> Decoder Msg
keyDecoder wrapper =
    JD.field "key" JD.string
        |> JD.map wrapper


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown <| keyDecoder (DownKey False)
        , Events.onKeyUp <| keyDecoder UpKey
        , Events.onKeyPress <| keyDecoder PressKey
        , confirmAnswer answerConfirmed
        , multiConfirmAnswer multiAnswerConfirmed
        , promptAnswer promptAnswerConfirmed
        , receiveGame ReceiveGame
        , deviceReady DeviceReady
        , Events.onResize UpdateWindowSize
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



{-
   showValue : a -> Html Msg
   showValue seed =
       div [] [ text <| Debug.toString seed ]
-}


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
    span [ onClick msg ]
        [ input
            [ type_ "radio"
            , name "board-size"
            , checked isChecked
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
            MainPage ->
                mainPageDiv model

            HelpPage ->
                helpPageDiv model

            TacticsPage ->
                tacticsPageDiv model

            CreditsPage ->
                creditsPageDiv model
        ]


errorMessageHtml : Model -> Html Msg
errorMessageHtml model =
    case model.message of
        Nothing ->
            text ""

        Just txt ->
            span [ class ErrorClass ]
                [ text <| "(" ++ txt ++ ")"
                ]


renderStarMenu : Model -> Html Msg
renderStarMenu model =
    let
        gameState =
            model.gameState

        showPossibilities =
            gameState.flags.showPossibilities

        exploreState =
            gameState.exploreState

        makeButton =
            \( msg, txt ) ->
                button
                    [ onClick msg
                    , class StarMenuButtonClass
                    ]
                    [ text txt ]

        possibilitiesButton =
            makeButton
                ( ToggleShowPossibilities
                , if showPossibilities then
                    "Hide row/col Possibilities"

                  else
                    "Show row/col Possibilities"
                )

        exploreLabels =
            case exploreState of
                Nothing ->
                    [ ( StartExploration, "Start Exploration" ) ]

                Just _ ->
                    [ ( KeepExploration, "Keep Exploration" )
                    , ( DiscardExploration, "Discard Exploration" )
                    ]

        exploreHtml =
            List.map makeButton exploreLabels

        keyClickButton =
            case model.platform of
                WebPlatform ->
                    []

                _ ->
                    [ makeButton
                        ( ToggleKeyClick
                        , if model.gameState.flags.keyClickSound then
                            "Disable Key Clicks"

                          else
                            "Enable Key Clicks"
                        )
                    ]

        cancelButton =
            makeButton ( CloseStarMenu, "Cancel" )

        topStyles =
            case model.boardSizes of
                Nothing ->
                    []

                Just sizes ->
                    let
                        pixels =
                            String.fromInt sizes.boardSize ++ "px"
                    in
                    [ ( "margin", pixels ++ " auto" )
                    , ( "width", "15em" )
                    ]
    in
    modalDiv CloseStarMenu
        []
        (List.map (\( name, value ) -> style name value) topStyles)
        (List.concat
            [ [ possibilitiesButton ]
            , keyClickButton
            , exploreHtml
            , [ cancelButton ]
            ]
        )


mainPageDiv : Model -> Html Msg
mainPageDiv model =
    div
        [ style "margin-top"
            ((String.fromInt <| BoardSize.iosTopPad model) ++ "px")
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
            , input
                [ value <| String.fromInt model.index
                , disabled <| restrictBoards model
                , type_ "number"
                , size 3
                , onInput NewBoardIndex
                , class ControlsClass
                , style "width" "2em"
                ]
                []
            , br
            , errorMessageHtml model

            -- , text (" " ++ toString model.time)  -- Will eventually be timer
            -- , showValue model.seed               -- debugging
            ]
        , div [] [ RenderBoard.render model ]
        , div [] [ RenderBoard.renderKeypad model ]
        , if model.showStarMenu then
            renderStarMenu model

          else
            text ""
        ]


linkDict : Dict String String
linkDict =
    Dict.fromList
        [ ( "Kakuro Dojo", "https://kakuro-dojo.com/" )
        , ( "Gib Goy Games", "https://GibGoyGames.com/" )
        , ( "Elm", "http://elm-lang.org/" )
        , ( "Haskell", "https://www.haskell.org/" )
        , ( "Lisp", "https://en.wikipedia.org/wiki/Lisp_(programming_language)" )
        , ( "hacker", "http://www.catb.org/hacker-emblem/" )
        , ( "Apache Cordova", "https://cordova.apache.org/" )
        , ( "FastClick", "https://ftlabs.github.io/fastclick/" )
        , ( "js-sha256", "https://github.com/emn178/js-sha256" )
        , ( "JavaScript", "https://en.wikipedia.org/wiki/JavaScript" )
        , ( "Paypal", "https://www.paypal.com/" )
        ]


lookupLink : String -> Maybe String
lookupLink linkText =
    Dict.get linkText linkDict


convertOneLink : String -> String -> ( List (Html Msg), String )
convertOneLink prefix linkPlus =
    let
        lp =
            String.split "]" linkPlus

        joinem =
            \() ->
                ( [ text prefix, text "[" ]
                , linkPlus
                )
    in
    case List.tail lp of
        Nothing ->
            joinem ()

        Just tail ->
            let
                linkText =
                    listHead lp ""
            in
            case lookupLink linkText of
                Nothing ->
                    joinem ()

                Just link ->
                    ( [ text prefix
                      , a [ href link ]
                            [ text linkText ]
                      ]
                    , String.join "]" tail
                    )


convertLinksLoop : String -> String -> List String -> List (List (Html Msg)) -> List (Html Msg)
convertLinksLoop prefix linkPlus tail res =
    let
        ( html, nextPrefix ) =
            convertOneLink prefix linkPlus

        res2 =
            html :: res
    in
    case List.head tail of
        Nothing ->
            List.append
                (List.concat (List.reverse res2))
                [ text nextPrefix ]

        Just nextLinkPlus ->
            convertLinksLoop
                nextPrefix
                nextLinkPlus
                (listTail tail)
                res2


listTail : List a -> List a
listTail list =
    case List.tail list of
        Nothing ->
            []

        Just tail ->
            tail


listHead : List a -> a -> a
listHead list default =
    case List.head list of
        Nothing ->
            default

        Just x ->
            x


convertLinks : String -> Html Msg
convertLinks string =
    let
        segs =
            String.split "[" string
    in
    case List.tail segs of
        Nothing ->
            text string

        Just tail ->
            case List.head tail of
                Nothing ->
                    text string

                --nothing after "["
                Just tail1 ->
                    span []
                        (convertLinksLoop
                            (listHead segs "")
                            tail1
                            (listTail tail)
                            []
                        )


pFormat : String -> List (Html Msg)
pFormat string =
    case String.left 1 string of
        "\t" ->
            [ blockquote [] (pFormat <| String.dropLeft 1 string) ]

        _ ->
            String.split "\n" string
                |> List.map convertLinks
                |> List.intersperse br
                |> (\x -> [ p [] x ])


ps : List String -> Html Msg
ps strings =
    List.map pFormat strings
        |> List.concat
        |> div [ class HelpTextClass ]


helpWindowSize : Int -> Int -> Model -> WindowSize
helpWindowSize num denom model =
    let
        windowSize =
            case model.windowSize of
                Nothing ->
                    { width = 256, height = 512 }

                Just sz ->
                    sz

        width =
            min windowSize.width windowSize.height

        size =
            num * min width 600 // denom
    in
    { width = size
    , height = 2 * size
    }


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
    a
        [ href "#"
        , onClick <| ShowPage page
        , title linkTitle
        ]
        [ text linkText ]


renderHelp : String -> Model -> WindowSize -> Html Msg
renderHelp name model size =
    p []
        [ case model.helpModelDict of
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
    case model.platform of
        WebPlatform ->
            "click/tap"

        _ ->
            "tap"


type alias PageSpec =
    ( String, Page, String )


textPageSpecs : List PageSpec
textPageSpecs =
    [ ( "Help", HelpPage, "Show the Help page." )
    , ( "Tactics", TacticsPage, "Show the Tactics page." )
    , ( "Credits", CreditsPage, "Show the Credits page." )
    ]


pageLinksLoop : String -> List PageSpec -> List (Html Msg) -> List (Html Msg)
pageLinksLoop pTitle specsTail res =
    case specsTail of
        [] ->
            List.intersperse space <| List.reverse res

        head :: tail ->
            let
                ( title, page, description ) =
                    head
            in
            if title /= pTitle then
                pageLinksLoop
                    pTitle
                    tail
                <|
                    pageLink page title description
                        :: res

            else
                pageLinksLoop pTitle tail <|
                    text title
                        :: res


textPageDiv : String -> Model -> List (Html Msg) -> Html Msg
textPageDiv pTitle model body =
    let
        pageLinks =
            pageLinksLoop pTitle textPageSpecs []
    in
    div []
        [ h2 [] [ text "Kakuro Dojo" ]
        , div []
            (List.append
                [ errorMessageHtml model
                , p [] <|
                    List.append [ playButton, br, br ] pageLinks
                , h3 [] [ text pTitle ]
                ]
             <|
                List.append body
                    [ p []
                        (List.append
                            pageLinks
                            [ br, br, playButton ]
                        )
                    ]
            )
        , footerDiv model
        ]


helpPageDiv : Model -> Html Msg
helpPageDiv model =
    let
        windowSize =
            helpWindowSize 1 2 model
    in
    textPageDiv "Help" model <|
        [ h3 [] [ text "Top of Page Controls" ]
        , ps
            [ "Number radio buttons to change size.\n'<' or '>' to change boards\n'X' to erase board.\n'X' on an empty board to erase all.\n'Help' link for this page."
            ]
        , h3 [] [ text "Board" ]
        , ps
            [ case model.platform of
                WebPlatform ->
                    "Click/Tap to select cell."

                _ ->
                    "Tap to select cell."
            ]
        , h3 [] [ text "Keypad" ]
        , ps
            [ case model.platform of
                WebPlatform ->
                    "Arrows, WASD, or IJKL to move.\n1-9 to enter number.\n0 or <space> to erase.\n'#' toggles hint input.\n'*' brings up the Exploratory Menu, described below."

                _ ->
                    "Arrows to move.\n1-9 to enter number.\n<blank> to erase.\n'#' toggles hint input.\n'*' brings up the Exploratory Menu, described below."
            ]
        , h3 [] [ text "Rules" ]
        , ps
            [ "Each contiguous row or column of white squares must contain unique numbers from 1 to 9. The numbers must sum to the number in the gray square to the left of a row or above a column."
            , "If you repeat a number, or fill a row or column with numbers with an incorrect sum, the possibly wrong numbers will be highlighted in red."
            , "When you tap '#' to enter hint input mode, you can enter multiple numbers that might be in a square, then use those to eliminate possibilities."
            , "(The boards below are \"live\". If you " ++ clickTap model ++ " a cell, the row and column possibilities will display below the board.)"
            ]
        , renderHelp "help1" model windowSize
        , renderHelp "help2" model windowSize
        , p []
            [ text "Also see: "
            , a
                [ href "https://en.wikipedia.org/wiki/Kakuro"
                , target "_blank"
                ]
                [ text "en.wikipedia.org/wiki/Kakuro" ]
            ]
        , h3 [] [ text "Exploratory menu" ]
        , ps
            [ "When you can go no farther without guesswork, the Exploratory Menu allows you to test and track various possibilities."
            , "Click \"*\" to start or end exploratory mode (as well as to enable or disable the row/col possibilities display)."
            , "Click \"Cancel\" or anywhere outside the menu to dismiss it without doing anything."
            , "Click \"Hide/Show row/col Possibilities\" to toggle the possibilities output between the game board and the keypad. If one of those rows gets too long, it will scroll horizontally."
            ]
        , case model.platform of
            WebPlatform ->
                text ""

            _ ->
                ps
                    [ "Click \"Enable/Disable Key Clicks\" to toggle whether tapping on the board or the keypad makes little click sounds."
                    ]
        , ps
            [ "Click \"Start Exploration\" to enter exploratory mode. In exploratory mode, the keypad numbers change to light blue color, and your guesses are displayed in that color. You can use it to make some exploratory guesses."
            , "If you decide to keep those guesses, bring up the menu again and click \"Keep Exploration\". The board will remain as it is, but with all the blue numbers changed to black."
            , "If you decide that the guesses were wrong, click \"Discard Exploration\", and the board will be returned to as it was when you clicked \"Start Exploration\". The cell where you made your first exploratory guess will be selected, and the keypad number of that guess will be shown in blue, to remind you of how you started the exploration."
            , "Exploratory mode is rarely necessary. Usually, a combination of hint numbers and logic will be enough to solve a puzzle."
            ]
        ]


tacticsPageDiv : Model -> Html Msg
tacticsPageDiv model =
    let
        windowSize =
            helpWindowSize 1 2 model

        bigWindowSize =
            helpWindowSize 3 4 model
    in
    textPageDiv "Tactics" model <|
        [ ps
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
            , "(The boards below are \"live\". If you " ++ clickTap model ++ " a cell, the row and column possibilities will display below the board.)"
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
            , a
                [ href "http://www.kakuro.com/howtoplay.php"
                , target "_blank"
                ]
                [ text "kakuro.com: How to Play" ]
            ]
        ]


creditsPageDiv : Model -> Html Msg
creditsPageDiv model =
    textPageDiv "Credits"
        model
        [ ps
            [ "[Kakuro Dojo] was written by Bill St. Clair, the proprietor of [Gib Goy Games]. I fell in love with Kakuro and wanted some features I couldn't find elsewhere. Then I discovered [Elm], and it became a labor of love. I hope you enjoy it as much as I've enjoyed making and playing it."
            , "[Kakuro Dojo] is written primarily in the [Elm] programming language. Elm is a pure functional language, similar to [Haskell], which compiles into [JavaScript], and has a very nice programming model. If an Elm program compiles, it will almost certainly never encounter an unexpected run-time error. Thank you to Evan Czaplicki, and the Elm community, for creating my favorite programming language (and that's a big compliment from this old [Lisp] [hacker])."
            ]
        , case model.platform of
            WebPlatform ->
                text ""

            _ ->
                ps
                    [ "[Apache Cordova] is a system that makes it very easy to distribute a web app as an App Store app. It is copyright The Apache Software Foundation and distributed under the Apache License, Version 2.0."
                    ]
        , ps
            [ "[FastClick] is a [JavaScript] library to eliminate a 300ms delay introduced by mobile web browsers. It is copyright The Financial Times Limited and distributed under the MIT License."
            , "[js-sha256] is a [JavaScript] library that computes the SHA256 cryptographic hash of a string. It is copyright Yi-Cyuan Chen and distributed under the MIT License."
            ]
        ]


footerDiv : Model -> Html Msg
footerDiv model =
    div [ id FooterId ]
        [ text "Play online at "
        , a [ href "https://kakuro-dojo.com/" ]
            [ text "kakuro-dojo.com" ]
        , br
        , text (copyright ++ " 2016-2025 ")
        , a [ href "https://GibGoyGames.com/" ]
            [ text "Gib Goy Games" ]
        , space
        , mailLink "GibGoyGames@gmail.com"
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
        , span []
            (case model.platform of
                WebPlatform ->
                    []

                _ ->
                    [ space
                    , logoLink "https://cordova.apache.org/"
                        "cordova-logo-84x81.png"
                        "App Made with Cordova"
                        32
                    ]
            )
        , space
        , logoLink "http://elm-lang.org/"
            "elm-logo-125x125.png"
            "Elm inside"
            28
        ]
