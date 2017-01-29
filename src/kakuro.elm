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

import SharedTypes exposing ( SavedModel, Model, GameState, ExploreState
                            , Msg, Msg(..), Page(..)
                            , IntBoard, HintsBoard, Selection, Flags
                            , MaybeHelpModelDict(..)
                            , IapProduct, IapPurchase, IapState
                            , Platform(..)
                            )
import Styles.Page exposing (id, class, PId(..), PClass(..))
import Board exposing (Board)
import PuzzleDB
import Entities exposing (nbsp, copyright)
import BoardSize
import DebuggingRender
import RenderBoard
import EncodeDecode exposing ( encodeGameState, encodeSavedModel
                             , decodeGameState, decodeSavedModel
                             , encodeIapStates, decodeIapStates
                             )
import HelpBoards exposing ( helpBoards )
import ModalDialog exposing ( modalDiv )

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
                     , div, p, h2, h3, h4, text, blockquote
                     , table, tr, td, th
                     , input, button, a, img, span, fieldset, label
                     )
import Html.Attributes exposing ( style, align, value, size
                                , href, target, src, title, alt
                                , width, height
                                , type_, size, placeholder
                                , name, checked
                                , colspan, disabled
        )
import Html.Events exposing (onClick, onInput)
import Keyboard exposing (KeyCode)
import Window
import Date
import Date.Extra as DE

main : Program (String, List (String, String), Maybe String) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }

-- (reason, string)
port specHash : (String, String) -> Cmd a
-- (reason, string, hash)
port receiveSpecHash : ((String, String, String) -> msg) -> Sub msg

port setStorage : Maybe String -> Cmd a

port saveGame : (String, String) -> Cmd msg

port requestGame : String -> Cmd msg
port receiveGame : (Maybe String -> msg) -> Sub msg

-- getProperty is intentionally missing.
-- The properties are all cached in the Model.
-- Writes have to go to both, but reads can come from the Model.
port setProperty : (String, Maybe String) -> Cmd msg

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

-- productIds
port iapGetProducts : List String -> Cmd msg
-- (products, error)
port iapProducts : ((Maybe (List IapProduct), Maybe String) -> msg) -> Sub msg

-- productId
port iapBuy : String -> Cmd msg
-- (productId, transactionId, error)
port iapBuyResponse : ((String, Maybe String, Maybe String) -> msg) -> Sub msg

port iapRestorePurchases : () -> Cmd msg
-- (purchases, error)
port iapPurchases : ((Maybe (List IapPurchase), Maybe String) -> msg) -> Sub msg

port deviceReady : (String -> msg) -> Sub msg

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

iapStatePropertyName : String
iapStatePropertyName =
    "iapStates"

propertiesToIapState : Dict String String -> Result String (Maybe (Dict String IapState))
propertiesToIapState properties =
    case Dict.get iapStatePropertyName properties of
        Nothing -> Ok Nothing
        Just json ->
            case decodeIapStates json of
                Err msg ->
                    Err msg
                Ok states ->
                    List.map (\x -> (x.product.productId, x)) states
                        |> Dict.fromList
                        |> Just
                        |> Ok

updateIapState : IapState -> Model -> (Model, Cmd Msg)
updateIapState state model =
    let productId = state.product.productId
        dict = case model.iapState of
                   Nothing -> Dict.empty
                   Just d -> d
        dict2 = Dict.insert productId state dict
        json = encodeIapStates
                 <| List.map (\(_, state) -> state)
                 <| Dict.toList dict2
    in
        ( { model | iapState = Just dict2 }
          , setProperty (iapStatePropertyName, Just json)
        )

decodePlatform : String -> Platform
decodePlatform name =
    case name of
        "iOS" -> IosPlatform
        "Android" -> AndroidPlatform
        _ -> WebPlatform

init : (String, List (String, String), Maybe String) -> ( Model, Cmd Msg )
init state =
    let (platformName, properties, maybeJson) = state
        platform = decodePlatform platformName
        savedModel = case maybeJson of
                       Nothing -> Nothing
                       Just json ->
                           case decodeSavedModel json of
                             Err _ -> Nothing
                             Ok savedModel ->
                                 Just savedModel
        propertiesDict = Dict.fromList properties
        iapRes = propertiesToIapState propertiesDict
        (iapState, message) =
            case iapRes of
                Err msg -> (Nothing, Just msg)
                Ok state -> (state, Nothing)
        mod = case savedModel of
                  Nothing -> { initialModel
                                 | platform = platform
                                 , properties = propertiesDict
                                 , iapState = iapState
                                 , message = message
                             }
                  Just m ->
                      let res = SharedTypes.savedModelToModel m
                      in
                          { res
                              | helpModelDict = Javole helpBoards
                              , platform = platform
                              , properties = propertiesDict
                              , iapState = iapState
                              , message = message
                          }
    in
      ( mod
      , Cmd.batch
          [ windowSizeCmd
          , setTitle pageTitle
          , seedCmd
          , maybeFetchProducts mod.page mod
          ]
      )

windowSizeCmd : Cmd Msg
windowSizeCmd =
    Task.perform (\x -> WindowSize x) Window.size

initialModel : Model
initialModel =
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
        , showStarMenu = False
        , helpModelDict = Javole helpBoards
        , platform = WebPlatform
        , properties = Dict.fromList []
        , deviceReady = False
        , iapState = Nothing
        , iapProducts = Nothing
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
        flags = gameState.flags
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
    let elts = String.split "," idStr
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
        flags = gameState.flags
    in
        case newSelection of
            Nothing -> model
            _ ->
                { model
                    | gameState =
                        { gameState
                            | selection = newSelection
                            , flags = { flags | firstGuess = 0 }
                        }
                    , message = Nothing
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

updateExploreState : GameState -> Int -> Int -> Int -> Maybe ExploreState
updateExploreState gameState row col digit =
    case gameState.exploreState of
        Nothing -> Nothing
        Just state ->
            let st2 = { state
                          | guesses = Board.set row col digit state.guesses
                      }
                st3 = if st2.firstGuess /= 0 then
                          st2
                      else
                          { st2
                              | firstGuess = digit
                              , firstGuessSelection = gameState.selection
                          }
            in
                Just st3                           
                           
processDigitKeys : Int -> Model -> Model
processDigitKeys keyCode model =
    let gameState = model.gameState
        selection = gameState.selection
        guesses = gameState.guesses
        hints = gameState.hints
        flags = gameState.flags
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
                                        , flags = { flags | firstGuess = 0 }
                                        , exploreState =
                                            updateExploreState
                                              gameState row col digit
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
            { model | showStarMenu = True }
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
        if forbidBoard board model then
            let m2 = { model | page = AdvertisePage }
            in
                if forbidBoard m2.gameState.board m2 then
                    getBoard 6 1 m2
                else
                    ( m2, Cmd.none )
        else
            gotoBoard board model

gotoBoard : IntBoard -> Model -> ( Model, Cmd a )
gotoBoard board model =
    case board.spec of
        Nothing ->
            let gameState = RenderBoard.makeGameState board
                idx = realBoardIndex board
                kind = board.rows
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
    let m = toggleFlag
              .showPossibilities (\v r -> { r | showPossibilities = v }) model
    in
        { m | showStarMenu = False }

startExploration : Model -> Model
startExploration model =
    let gameState = model.gameState
        guesses = gameState.guesses
        kind = guesses.rows
        exploreState = { savedBoard = guesses
                       , savedHints = gameState.hints
                       , guesses = Board.make kind kind 0
                       , firstGuess = 0
                       , firstGuessSelection = gameState.selection
                       }
        newState = { gameState | exploreState = Just exploreState }
    in
        { model
            | gameState = newState
            , showStarMenu = False
        }

keepExploration : Model -> Model
keepExploration model =
    let gameState = model.gameState
        flags = gameState.flags
    in
        { model
            | gameState = { gameState
                              | exploreState = Nothing
                              , flags = { flags | firstGuess = 0 }
                          }
            , showStarMenu = False}

discardExploration : Model -> Model
discardExploration model =
    let gameState = model.gameState
    in
        case gameState.exploreState of
            Nothing -> model
            Just state ->
                let selection = case state.firstGuessSelection of
                                    Nothing -> gameState.selection
                                    Just sel -> Just sel
                    flags = gameState.flags
                    newState = { gameState
                                   | guesses = state.savedBoard
                                   , hints = state.savedHints
                                   , flags = { flags
                                                 | firstGuess = state.firstGuess }
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

unlockDateReason : String
unlockDateReason =
    "unlockDate"

specHashDict : Dict String (String -> String -> Model -> Model)
specHashDict =
    Dict.fromList [ ( unlockDateReason, updateUnlockDateHash )
                  ]

specHashReceiver : (String, String, String) -> Msg
specHashReceiver (reason, string, hash) =
    case Dict.get reason specHashDict of
        Nothing -> Nop
        Just r ->
            InvokeSpecHashReceiver r string hash

updateUnlockDateHash : String -> String -> Model -> Model
updateUnlockDateHash unlockDate hash model =
    let times = model.times
    in
        { model |
          times = { times |
                    unlockDate = unlockDate
                  , unlockHash = hash
                  }
        }

convertTimeToUnlockDate : Time -> String
convertTimeToUnlockDate time =
    let date = Date.fromTime time
    in
        DE.toFormattedString "yyMMdd" date

timeTick : Time -> Model -> ( Model, Cmd Msg )
timeTick time model =
    let times = model.times
        unlockDate = convertTimeToUnlockDate time
        cmd = if unlockDate /= times.unlockDate then
                  specHash (unlockDateReason, unlockDate)
              else
                  Cmd.none
                  
    in
        ( { model |
            times = { times | timestamp = time }
          }
        , cmd
        )

timePlayTick : Time -> Model -> ( Model, Cmd Msg)
timePlayTick time model =
    let (model2, cmd) = timeTick time model
        gameState = model2.gameState
        gameStateTimes = gameState.times
    in
        ( { model2 |
            gameState = { gameState |
                          times = { gameStateTimes |
                                    elapsed = gameStateTimes.elapsed + 1
                                  }
                        }
          }
        , cmd
        )

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

gotDeviceReady : Model -> String -> ( Model, Cmd Msg )
gotDeviceReady model platformName =
    let platform = decodePlatform platformName
        m = { model
                | deviceReady = True
                , platform = platform
            }
        m2 = addBoardSizesToModel m 
    in
        ( m2
        , if m2.page == IapPage then
              iapGetProducts iapProductIds
          else
              Cmd.none
    )

extraPuzzlesProductId : String
extraPuzzlesProductId =
    "puzzles2"

-- This must match the App Store setup
-- You can't simply add IDs here.
-- iapPageDiv assumes there is only one product.
-- I was too lazy to do it right, since I don't expect there to ever
-- be another one.
iapProductIds : List String
iapProductIds =
    [ extraPuzzlesProductId
    ]

maybeFetchProducts : Page -> Model -> Cmd Msg
maybeFetchProducts page model =
    if page == IapPage
        && model.iapProducts == Nothing
        && model.iapState == Nothing
        && model.deviceReady
    then
        iapGetProducts iapProductIds
    else
        Cmd.none

iapProductsDict : Model -> Dict String IapProduct
iapProductsDict model =
  case model.iapProducts of
      Nothing -> Dict.empty
      Just (products, _) ->
          case products of
              Nothing -> Dict.empty
              Just list -> Dict.fromList
                           <| List.map (\x -> (x.productId, x)) list

processIapBuy : Model -> String -> Maybe String -> Maybe String -> (Model, Cmd Msg)
processIapBuy model productId transactionId error =
    let m = { model | message = Nothing }
    in
        case transactionId of
            Nothing ->
                doIapBuy model Nothing error
            Just tid ->
                doIapBuy
                    model
                    (Just <| IapPurchase productId tid model.times.timestamp)
                        Nothing

doIapBuy : Model -> Maybe IapPurchase -> Maybe String -> (Model, Cmd Msg)
doIapBuy model purchase error =
    case purchase of
        Nothing ->
            ( { model | message = error }, Cmd.none )
        Just p ->
            let { productId, transactionId, date } = p
            in
                case Dict.get productId <| iapProductsDict model of
                    Nothing ->
                        ( { model |
                            message = Just ("productId not found: \"" ++ productId ++ "\"")
                          }
                        , Cmd.none
                        )
                    Just product ->
                        let state = { product = product
                                    , purchase = p
                                    }
                        in
                            updateIapState state model

receiveProducts : (Maybe (List IapProduct), Maybe String) -> Model -> Model
receiveProducts prodsAndErr model =
    let (prods, err) = prodsAndErr
    in
        case err of
            Just str ->
                { model |
                  iapProducts = Just (Nothing, err)
                }
            Nothing ->
                case prods of
                    Nothing ->
                        { model |
                          iapProducts = Just (Nothing, Just "Missing products.")
                        }
                    Just products ->
                        case List.filter (\x -> x.productId /= "") products of
                            [] ->
                                { model |
                                  iapProducts =
                                      Just (Nothing, Just "No products returned.")
                                }
                            ps ->
                                { model |
                                 iapProducts = Just (Just ps, Nothing)
                                }

iapPurchasesLoop : List IapPurchase -> Model -> Cmd Msg -> (Model, Cmd Msg)
iapPurchasesLoop purchases model cmd =
    case purchases of
        [] -> ( model, cmd )
        p :: tail ->
            let (m, c) = doIapBuy model (Just p) Nothing
            in
                iapPurchasesLoop tail m
                    <| if c == Cmd.none then cmd else c
                
processIapPurchases : Model -> Maybe (List IapPurchase) -> Maybe String -> ( Model, Cmd Msg)
processIapPurchases model purchases error =
    let m = { model | message = Nothing }
    in
        case purchases of
            Nothing ->
                doIapBuy model Nothing error
            Just [] ->
                doIapBuy model Nothing (Just "You have no purchases to restore.")
            Just ps ->
                iapPurchasesLoop ps model Cmd.none

updateHelpPage : Msg -> Model -> ( Model, Cmd Msg)
updateHelpPage msg model =
    case msg of
        ShowPage page ->
            ( { model | page = page, message = Nothing }
            , maybeFetchProducts page model
            )
        ReceiveGame maybeJson ->
            receiveGameJson maybeJson model
        ReloadIapProducts ->
            ( { model | iapProducts = Nothing }
            , iapGetProducts iapProductIds
            )
        RestoreIapPurchases ->
            ( model,
              iapRestorePurchases()
            )
        Tick time ->
            timeTick time model
        Seed time ->
            doSeed time model
        ClickCell id ->
            ( updateSelectedCell id model, Cmd.none )
        WindowSize size ->
            processWindowSize model size
        DeviceReady platformName ->
            gotDeviceReady model platformName
        IapProducts products ->
            ( receiveProducts products model
            , Cmd.none
            )
        IapBuy productId ->
            ( { model | message = Nothing }
            , iapBuy productId )
        IapBuyResponse (productId, transactionId, error) ->
            processIapBuy model productId transactionId error
        IapPurchases (purchases, error) ->
            processIapPurchases model purchases error
        InvokeSpecHashReceiver receiver string hash ->
            ( receiver string hash model
            , Cmd.none)
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
            if (model.platform == WebPlatform) &&
               (Board.isBoardEmpty gameState.guesses) &&
               (Board.isBoardEmpty gameState.hints) then
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
    ( restartQuery, "Confirm", [ "Cancel", "Clear All Saved Puzzles", "Restart This Puzzle" ] )

-- TBD
resetAllGameStates : Model -> ( Model, Cmd Msg )
resetAllGameStates oldModel =
    ( { initialModel
      | times = oldModel.times
      , windowSize = oldModel.windowSize
      , seed = oldModel.seed
      , platform = oldModel.platform
      , iapState = oldModel.iapState
      , iapProducts = oldModel.iapProducts
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
    case model.platform of
        WebPlatform ->
            confirmDialog restartQuery
        _ ->
            multiConfirmDialog multiRestartQuery

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

restrictBoards : Model -> Bool
restrictBoards model =
    case model.iapState of
        Nothing -> True
        Just dict ->
            case Dict.get extraPuzzlesProductId dict of
                Nothing -> True
                Just _ -> False

forbidBoard : IntBoard -> Model -> Bool
forbidBoard board model =
    case board.index of
        Nothing -> False
        Just index ->
            let kind = board.rows
            in
                (restrictBoards model) && (kind > 8 || index > 5)        

doSeed : Time -> Model -> ( Model, Cmd Msg )
doSeed time model =
    let m = { model | seed = Just <| Random.initialSeed (round time) }
    in
        timeTick time model

processNewBoardIndex : String -> Model -> ( Model, Cmd Msg )
processNewBoardIndex indexStr model =
    case String.toInt <| String.right 2 indexStr of
        Err _ -> ( model, Cmd.none )
        Ok index ->
            let maxidx = PuzzleDB.numberOfBoardsOfKind model.kind
                idx = if index >= maxidx then
                          case String.toInt <| String.right 1 indexStr of
                              Err _ -> index
                              Ok i ->
                                  if i < 1 then 1 else i
                      else
                          index
            in
                getBoard (log "model.kind" model.kind) (log "idx" idx) model

updateMainPage : Msg -> Model -> ( Model, Cmd Msg )
updateMainPage msg model =
    case msg of
        ShowPage page ->
            ( { model | page = page, message = Nothing }
            , maybeFetchProducts page model
            )
        ReloadIapProducts ->
            ( model, Cmd.none )
        RestoreIapPurchases ->
            ( model, Cmd.none )
        ChangeKind kind ->
            getBoard kind (getKindIndex kind model) model
        Generate increment ->
            getBoard model.kind (model.index + increment) model
        NewBoardIndex indexStr ->
            processNewBoardIndex indexStr model
        Restart ->
            ( model, restartDialog model )
        Tick time ->
            timePlayTick time model
        Seed time ->
            doSeed time model
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
        OpenStarMenu ->
            ( { model | showStarMenu = True }, Cmd.none )
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
        IapProducts products ->
            ( receiveProducts products model
            , Cmd.none
            )
        IapBuy productId ->
            ( { model | message = Nothing }
            , iapBuy productId )
        IapBuyResponse (productId, transactionId, error) ->
            processIapBuy model productId transactionId error
        IapPurchases (purchases, error) ->
            processIapPurchases model purchases error
        InvokeSpecHashReceiver receiver string hash ->
            ( receiver string hash model
            , Cmd.none)
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
    Sub.batch
        [ Time.every second Tick
        , Keyboard.downs DownKey
        , Keyboard.ups UpKey
        , Keyboard.presses PressKey
        , confirmAnswer answerConfirmed
        , multiConfirmAnswer multiAnswerConfirmed
        , promptAnswer promptAnswerConfirmed
        , receiveGame ReceiveGame
        , deviceReady DeviceReady
        , iapProducts IapProducts
        , iapBuyResponse IapBuyResponse
        , iapPurchases IapPurchases
        , receiveSpecHash specHashReceiver
        , Window.resizes WindowSize
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
    span [ onClick msg]
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
              MainPage -> mainPageDiv model
              HelpPage -> helpPageDiv model
              TacticsPage -> tacticsPageDiv model
              CreditsPage -> creditsPageDiv model
              IapPage -> iapPageDiv model
              AdvertisePage -> advertisePageDiv model
        ]

errorMessageHtml : Model -> Html Msg
errorMessageHtml model =
    case model.message of
        Nothing -> text ""
        Just txt ->
            span [ class ErrorClass ]
                [ text <| "(" ++ txt ++ ")"
                ]

renderStarMenu : Model -> Html Msg
renderStarMenu model =
    let gameState = model.gameState
        showPossibilities = gameState.flags.showPossibilities
        exploreState = gameState.exploreState
        makeButton = (\(msg, txt) ->
                       button [ onClick msg
                              , class StarMenuButtonClass
                              ]
                           [ text txt ]
                     )
        possibilitiesButton = makeButton
                                ( ToggleShowPossibilities
                                , if showPossibilities then
                                      "Hide row/col possibilities"
                                  else
                                      "Show row/col possibilities")
        exploreLabels = case exploreState of
                            Nothing ->
                                [ (StartExploration, "Start Exploration") ]
                            Just _ ->
                                [ (KeepExploration, "Keep Exploration")
                                , (DiscardExploration, "Discard Exploration")
                                ]
        exploreHtml = List.map makeButton exploreLabels
        cancelButton = makeButton ( CloseStarMenu, "Cancel" )
        topStyles = case model.boardSizes of
                        Nothing -> []
                        Just sizes ->
                            let pixels = (toString sizes.boardSize) ++ "px"
                            in
                                [ ("margin", pixels ++ " auto")
                                , ("width", "20em")
                                ]
    in
        modalDiv CloseStarMenu
            []
            [ style topStyles ]
            ( List.append (possibilitiesButton :: exploreHtml)
                          [ cancelButton ]
            )

mainPageDiv : Model -> Html Msg
mainPageDiv model =
    div [ style [ ("margin-top"
                  , (toString <| BoardSize.iosTopPad model) ++ "px"
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
                  , disabled <| restrictBoards model
                  , type_ "number"
                  , size 3
                  , onInput NewBoardIndex
                  , class ControlsClass
                  , style [ ("width", "2em") ]
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
        , ( "hacker" , "http://www.catb.org/hacker-emblem/" )
        , ( "Apache Cordova", "https://cordova.apache.org/" )
        , ( "FastClick", "https://ftlabs.github.io/fastclick/" )
        , ( "js-sha256", "https://github.com/emn178/js-sha256" )
        , ( "JavaScript", "https://en.wikipedia.org/wiki/JavaScript" )
        , ( "Paypal", "https://www.paypal.com/" )
        ]

lookupLink : String -> Maybe String
lookupLink linkText =
    Dict.get linkText linkDict

convertOneLink : String -> String -> (List (Html Msg), String)
convertOneLink prefix linkPlus =
    let lp = String.split "]" linkPlus
        joinem = (\() ->
                   ( [ text prefix, text "[" ]
                   , linkPlus
                   )
                 )
    in
        case List.tail lp of
            Nothing -> joinem()
            Just tail ->
                let linkText = listHead lp ""
                in
                    case lookupLink linkText of
                        Nothing -> joinem()
                        Just link ->
                            ( [ text prefix
                             , a [ href link ]
                                  [ text linkText ]
                              ]
                            , String.join "]" tail
                            )

convertLinksLoop : String -> String -> List String -> List (List (Html Msg)) -> List (Html Msg)
convertLinksLoop prefix linkPlus tail res =
    let (html, nextPrefix) = convertOneLink prefix linkPlus
        res2 = html :: res
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
        Nothing -> []
        Just tail -> tail

listHead : List a -> a -> a
listHead list default =
    case List.head list of
        Nothing -> default
        Just x -> x

convertLinks : String -> Html Msg
convertLinks string =
    let segs = String.split "[" string
    in
        case List.tail segs of
            Nothing -> text string
            Just tail ->
                case List.head tail of
                    Nothing -> text string --nothing after "["
                    Just tail1 ->
                        span []
                            ( convertLinksLoop
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
    case model.platform of
        WebPlatform ->
            "click/tap"
        _ ->
            "tap"

type alias PageSpec =
    (String, Page, String)

textPageSpecs : List PageSpec
textPageSpecs =
    [ ( "Help", HelpPage, "Show the Help page.")
    , ( "Tactics", TacticsPage, "Show the Tactics page.")
    , ( "Credits", CreditsPage, "Show the Credits page.")
    , ( "Purchases", IapPage, "Show the Purchases page.")
    ]

pageLinksLoop : String -> List PageSpec -> List (Html Msg) -> List (Html Msg)
pageLinksLoop pageTitle specsTail res =
    case specsTail of
        [] ->
            List.intersperse space <| List.reverse res
        head :: tail ->
            let (title, page, description) = head
            in
                if title /= pageTitle then
                    pageLinksLoop
                        pageTitle tail
                            <| (pageLink page title description) :: res
                else
                    pageLinksLoop pageTitle tail
                        <| (text title) :: res

textPageDiv : String -> Model-> List (Html Msg) -> Html Msg
textPageDiv pageTitle model body =
    let pageLinks = pageLinksLoop pageTitle textPageSpecs []
    in
        div []
            [ h2 [] [ text "Kakuro Dojo" ]
            , div []
                ( List.append
                      [ errorMessageHtml model
                      , p []
                            <| List.append [ playButton , br , br ] pageLinks
                      , h3 [] [ text pageTitle ]
                      ]
                      <| List.append body
                      [ p []
                            ( List.append
                                  pageLinks
                                  [ br , br , playButton ]
                            )
                      ]
                )
            , footerDiv model
            ]
                
helpPageDiv : Model -> Html Msg
helpPageDiv model =
    let windowSize = helpWindowSize 1 2 model
    in
        textPageDiv "Help" model <|
            [ h3 [] [ text "Top of Page Controls" ]
            , ps [ "Number radio buttons to change size.\n'<' or '>' to change boards\n'X' to erase board.\n'Help' link for this page."
                 ]
            , h3 [] [ text "Board" ]
            , ps [ case model.platform of
                       WebPlatform ->
                         "Click/Tap to select cell."
                       _ ->
                         "Tap to select cell."
                 ]
            , h3 [] [ text "Keypad" ]
            , ps [ case model.platform of
                       WebPlatform ->
                         "Arrows, WASD, or IJKL to move.\n1-9 to enter number.\n0 or <space> to erase.\n'*' toggles row/col possibility display.\n'#' toggles hint input."
                       _ ->
                         "Arrows to move.\n1-9 to enter number.\n<blank> to erase.\n'*' toggles row/col possibility display.\n'#' toggles hint input."
                 ]
            , h3 [] [ text "Rules" ]
            , ps [ "Each contiguous row or column of white squares must contain unique numbers from 1 to 9. The numbers must sum to the number in the gray square to the left of a row or above a column."
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
            ]

tacticsPageDiv : Model -> Html Msg
tacticsPageDiv model =
    let windowSize = helpWindowSize 1 2 model
        bigWindowSize = helpWindowSize 3 4 model
    in
        textPageDiv "Tactics" model <|
            [ ps [ "You won't get very far by simply guessing numbers. It helps to use the hints, which are toggled by typing or tapping '#'. You will also learn to recognize clues which have a small number of possible solutions."
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
            , ps [ "To use this to solve a puzzle, first fill in the possibilities for the simple sums of two or three numbers, intersecting the lists where a row and column cross each other. Here's the example from the 'Help' page, with that done, using the fact, shown in the 'row' possibilities for the 8/3 row that '8/3 = 125 134'."
                 , "(The boards below are \"live\". If you " ++ (clickTap model) ++ " a cell, the row and column possibilities will display below the board.)"
                 ]
            , renderHelp "help1" model windowSize
            , ps [ "Next, replace the cells with only one hint number with a real guess, and eliminate that hint number from the other cells in its row and column:"
                 ]
            , renderHelp "tactics2" model windowSize
            , ps [ "For this simple example, just iterate until done: "
                 ]
            , renderHelp "tactics3" model windowSize
            , renderHelp "tactics4" model windowSize
            , p [] [ playButton ]
            , ps [ "A real example has more complicated sums, which you can't fill in right away, but if you use the possibilities display, you can often figure out what to do. For example, below is 6x6 board number 2, with the simple sum hints filled in, and using '15/5 = 12345'."
                 , "There are a few things to notice about that board."
                 , "The right cell of the 11/2 row in the upper-left-hand corner contains only '9', since 11 minus 1 is 10, which doesn't work. This means that the left cell can only contain '2', not '12' as is allowed by 3/2 = 12'. Similarly for the top cell of the 9/2 column in the lower-left-hand corner, and the top cell of the 15/2 column in the lower-right-hand corner."
                 , "The '15/5' column in the middle of the board does not have '12345' as guesses for all of its cells. That's because 4 is not a valid guess for '8/2', being half of 8, the minimum value for '12/2' is 3, and the minimum value for '13/2' is 4. Also the right-most cell of the '15/5' row near the top is missing a 3, since that is half of 6. If you know the numbers that can go in one cell of a 2-cell sum, the other cell's numbers are easy to compute, by subtracting each known number from the sum."
                 ]
            , renderHelp "board2Model1" model bigWindowSize
            , ps [ "Filling in the numbers with only one possibility, noticing that 1 is required for the '15/2' column in the center of the board, and it appears only in the '8/2' row, and removing the filled-in numbers from the possibilities in their rows and columns, gives:"
                 ]
            , renderHelp "board2Model2" model bigWindowSize
            , ps [ "The left cell of the '8/2' row in the middle of the board is the only cell in its '15/5' column containing a 1, so it must be 1:"
                 ]
            , renderHelp "board2Model3" model bigWindowSize
            , ps [ "There is a single blank cell in the '23/5' row near the bottom. We could proceed by noticing that the 2 in the '234' cell to its left is the only 2 in its '15/5' column, but I'm going to talk a little first about how to fill in a single blank cell. Add up the minimum possibilities in its row and column, subtract each from its total, and take the maximum, that's max(23-(1+2+8+3), 34-(5+3+7+8)) = max(9, 11) = 11. Add up the maximum possibilities in its row and column, subtract each from its total, and take the minimum, that's min(23-(1+4+8+3), 34-(6+5+7+9)) = max(7, 7) = 7. So the value in that empty cell needs to be between 7 and 11, i.e. 7, 8, or 9. Since 7 is already in its column, and 8 is in its row, it must be 9."
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
            ]

creditsPageDiv : Model -> Html Msg
creditsPageDiv model =
    textPageDiv "Credits" model
        [ ps [ "[Kakuro Dojo] was written by Bill St. Clair, the proprietor of [Gib Goy Games]. I fell in love with Kakuro and wanted some features I couldn't find elsewhere. Then I discovered [Elm], and it became a labor of love. I hope you enjoy it as much as I've enjoyed making and playing it."
             , "[Kakuro Dojo] is written primarily in the [Elm] programming language. Elm is a pure functional language, similar to [Haskell], which compiles into [JavaScript], and has a very nice programming model. If an Elm program compiles, it will almost certainly never encounter an unexpected run-time error. Thank you to Evan Czaplicki, and the Elm community, for creating my favorite programming language (and that's a big compliment from this old [Lisp] [hacker])."
             ]
        , ( case model.platform of
                WebPlatform ->
                  text ""
                _ ->
                  ps [ "[Apache Cordova] is a system that makes it very easy to distribute a web app as an App Store app. It is copyright The Apache Software Foundation and distributed under the Apache License, Version 2.0."
                     ]
          )
        , ps [ "[FastClick] is a [JavaScript] library to eliminate a 300ms delay introduced by mobile web browsers. It is copyright The Financial Times Limited and distributed under the MIT License."
             , "[js-sha256] is a [JavaScript] library that computes the SHA256 cryptographic hash of a string. It is copyright Yi-Cyuan Chen and distributed under the MIT License."
             ]
        ]

mergePurchasesIntoProducts : List (String, IapState) -> List IapProduct -> List IapProduct
mergePurchasesIntoProducts purchases products =
    case purchases of
        [] -> products
        (productId, state) :: tail ->
            case LE.find (\x -> productId == x.productId) products of
                Nothing -> mergePurchasesIntoProducts
                             tail <| state.product :: products
                Just _ -> mergePurchasesIntoProducts tail products

getIapState : Model -> (List IapProduct, Dict String IapPurchase, Maybe String)
getIapState model =
    let stateDict = case model.iapState of
                        Nothing -> Dict.empty
                        Just d -> d
        (products, error) = case model.iapProducts of
                                Nothing -> ([], Nothing)
                                Just mps ->
                                    case mps of
                                        (Nothing, err) ->
                                            case err of
                                                Nothing -> ([], Just "No products found.")
                                                x -> ([], x)
                                        (Just ps, _) -> (ps, Nothing)
        purchaseDict = Dict.toList stateDict
                     |> List.map (\x -> (Tuple.first x
                                        , (.purchase) <| Tuple.second x))
                     |> Dict.fromList
    in
        (mergePurchasesIntoProducts (Dict.toList stateDict) products
        , purchaseDict
        , error)

iapProductRow : IapProduct -> Maybe IapPurchase -> List (Html Msg)
iapProductRow product purchase =
    [ tr []
          [ td []
                [ text product.title
                ]
          , td []
              [ text product.price ]
          , td []
              [ case purchase of
                    Nothing ->
                      button [ onClick <| IapBuy product.productId
                             , class ControlsClass
                             , title "Buy this product."
                             ]
                             [ text "Buy" ]
                    Just p ->
                      text
                        -- "z" doesn't work here for time zone
                        <| DE.toFormattedString "d MMM y, h:mm a"
                        <| Date.fromTime p.date
              ]
        ]
    , tr [ ]
        [ td [ colspan 3 ]
              [ text product.description ]
        ]
    ]

iapPageDiv : Model -> Html Msg
iapPageDiv model =
    let (products, purchaseDict, error) = getIapState model
        productsPurchased = (Dict.size purchaseDict) > 0
    in
        textPageDiv "Purchases" model
        <| case model.platform of
               WebPlatform ->
                   webIapElements model productsPurchased
               _ ->
                   appIapElements model products purchaseDict error productsPurchased

rawPuzzleEnablerLink : Model -> String
rawPuzzleEnablerLink model =
    let hash = model.times.unlockHash
    in
        "kakuro-dojo.com/unlock/?hash=" ++ hash

puzzleEnablerLink : Model -> Html Msg
puzzleEnablerLink model =
  let link = rawPuzzleEnablerLink model
  in
      a [ href <| "https://" ++ link ]
          [ text link ]

webIapElements : Model -> Bool -> List (Html Msg)
webIapElements model productsPurchased =
    if productsPurchased then
        [ p [ class HelpTextClass]
              [ text "You have enabled all the puzzles."
              , br
              , text "To enable them in another browser, visit this link in that browser:"
              , br
              , puzzleEnablerLink model
              ]
        , p [ class HelpTextClass ]
            [ text "The link will only work today and tomorrow. Revisit this page after that, in this browser, to get a new link."
            ]
        , appStoreBlurb model
        ]
    else
        [ ps [ "You are using a web demo of the Kakuro Dojo app. The demo gives you only 10 puzzles, in 6x6 and 8x8 layouts. The app allows you to purchase 190 additional puzzles, in 6x6, 8x8, and 10x10 layouts, and will provide a link with which you can enable those additional puzzles in this web version."
             , "If you [Paypal] $0.99 (or more) to bill@billstclair.com, along with an email address, I'll send you a link to enable the additional puzzles."
             ]
        , appStoreBlurb model
        ]

appIapElements : Model -> List IapProduct -> Dict String IapPurchase -> Maybe String -> Bool -> List (Html Msg)
appIapElements model products purchaseDict error productsPurchased =
    let canRestore = case model.iapProducts of
                         Just (Just _, _) -> True
                         _ -> False
    in
        [ p []
            [ case model.iapProducts of
                  Nothing ->
                    text <| if productsPurchased then "" else "Fetching products..."
                  Just (prods, error) ->
                    case error of
                        Just msg ->
                          span []
                            [ text <| "Error loading products: " ++ msg
                            , br
                            , button
                                  [ onClick ReloadIapProducts
                                  , class ControlsClass
                                  ]
                                  [ text "Reload" ]
                            ]
                        Nothing ->
                          text ""
            ]
        , table [ class PrettyTable ]
            (( tr []
                   [ th [] [ text "Product" ]
                   , th [] [ text "Price" ]
                   , th [] [ text "Purchase" ]
                   ]
             ) ::
                 (List.concat
                      ( List.map
                            (\x -> iapProductRow x (Dict.get x.productId purchaseDict))
                            products
                      )
                 )
            )
        , if productsPurchased || (not canRestore) then
              if productsPurchased then
                  div [ class HelpTextClass ]
                      [ p []
                            [ text "Thank you for purchasing the additional puzzles. You may use them on other devices by pressing the \"Restore Purchases\" button that appears on this page on a device with a newly-installed version of Kakuro Dojo."
                            ]
                      , p []
                            [ text "To enable all the puzzles in a web browser, visit this link in that browser:"
                            , br
                            , puzzleEnablerLink model
                            ]
                      , p []
                          [ text "The link will only work today and tomorrow. Revisit this page after that to get a new link."
                          ]
                      ]
              else
                  text ""
          else
              p [ class HelpTextClass ]
                  [ text "Click the button below to restore purchases you made on another device or that you lost on this device by deleting the Kakuro Dojo app."
                  , br, br
                  , button
                        [ onClick RestoreIapPurchases
                        , class ControlsClass
                        ]
                        [ text "Restore Purchases" ]
                  ]
        ]

advertisePageDiv : Model -> Html Msg
advertisePageDiv model =
    textPageDiv "Commercial Message" model
        [ ps [ "This game is free to play for five boards of each of the 6x6 and 8x8 layouts. For a small fee, you can get 190 more boards, split between 6x6, 8x8, and 10x10 layouts."
             ]
        , p []
            [ button
                  [ onClick <| ShowPage IapPage
                  , class ControlsClass
                  ]
                  [ text "Go to Purchases Page" ]
            ]
        , appStoreBlurb model
        ]

appStoreBlurb : Model -> Html Msg
appStoreBlurb model =
    if model.platform /= WebPlatform then
        text ""
    else
        div [ class HelpTextClass ]
            [ p []
                  [ text "The game is also available as an app, for your portable device. Click a button below to go to the relevant App Store page. You can purchase the additional puzzles in the app."
                  ]
            , p []
                [ a [ href "https://itunes.apple.com/us/app/kakuro-dojo/id1191778737?mt=8" ]
                      [ img [ src "images/Download_on_the_App_Store_Badge_US-UK_135x40.svg"
                            , alt "Download on the App Store"
                            , width 135
                            , height 40
                            ]
                            []
                      ]
                , text (nbsp ++ nbsp)
                , a [ href "https://play.google.com/store/apps/details?id=com.gibgoygames.kakuro" ]
                        [ img [ src "images/google-play-badge.png"
                              , alt "Get it on Google Play"
                              , width 137
                              , height 40
                              ]
                              []
                        ]
                ]
            ]
            
footerDiv : Model -> Html Msg
footerDiv model =
    div [ id FooterId ]
      [ text "Play online at "
      , a [ href "https://kakuro-dojo.com/" ]
          [ text "kakuro-dojo.com" ]
      , br
      , text (copyright ++ " 2016-2017 ")
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
          ( case model.platform of
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
