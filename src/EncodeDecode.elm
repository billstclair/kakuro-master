----------------------------------------------------------------------
--
-- EncodeDecode.elm
-- Functions to encode state to JSON strings and decode it back into Elm
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module EncodeDecode exposing ( encodeGameState, encodeSavedModel
                             , decodeGameState, decodeSavedModel
                             , encodeIapStates, decodeIapStates)

import SharedTypes exposing ( Labels, Hints, Flags, Selection
                            , IntBoard, LabelsBoard, HintsBoard
                            , GameStateTimes
                            , GameState, Page(..), SavedModel
                            , IapState, IapProduct, IapPurchase
                            )
import Board exposing (Board)

import VersionedJson exposing ( ConverterDict
                              , encodeVersionedJson, decodeVersionedJson)

import Json.Decode as JD exposing (field, Decoder)
import Json.Encode as JE exposing (Value)
import Time exposing (Time)
import Dict exposing (Dict)
import Array

--
-- Encoders
--

xBoardEncoder : (x -> Value) -> Board x -> Value
xBoardEncoder xEncoder board =
    JE.object
        [ ("rows", JE.int board.rows)
        , ("cols", JE.int board.cols)
        , ("defaultValue", xEncoder board.defaultValue)
        , ("spec", case board.spec of
                     Nothing -> JE.null
                     Just spec -> JE.string spec)
        , ("index", case board.index of
                      Nothing -> JE.null
                      Just index -> JE.int index)
        , ("array", JE.array <|
               Array.map
                 (\x -> JE.array <| Array.map xEncoder x)
                 board.array)
        ]

intBoardEncoder : IntBoard -> Value
intBoardEncoder board =
    xBoardEncoder JE.int board

labelsEncoder : Labels -> Value
labelsEncoder labels =
    let (x, y) = labels
    in
      JE.list [ JE.int x, JE.int y ]

labelsBoardEncoder : LabelsBoard -> Value
labelsBoardEncoder board =
    xBoardEncoder labelsEncoder board

hintsEncoder : Hints -> Value
hintsEncoder hints =
    JE.list <| List.map JE.int hints

hintsBoardEncoder : HintsBoard -> Value
hintsBoardEncoder board =
    xBoardEncoder hintsEncoder board

flagsEncoder : Flags -> Value
flagsEncoder flags =
    JE.object
        [ ("isHintInput", JE.bool flags.isHintInput)
        , ("showPossibilities", JE.bool flags.showPossibilities)
        ]

selectionEncoder : Maybe Selection -> Value
selectionEncoder maybeSelection =
    case maybeSelection of
      Nothing -> JE.null
      Just (x, y) ->
          JE.list [ JE.int x, JE.int y ]

gameStateTimesEncoder : GameStateTimes2 -> Value
gameStateTimesEncoder times =
    JE.object
        [ ("timestamp", JE.float times.timestamp)
        , ("elapsed", JE.float times.elapsed)
        ]

gameStateEncoder : GameState2 -> Value
gameStateEncoder gameState =
    JE.object
        [ ("board", intBoardEncoder gameState.board)
        , ("labels", labelsBoardEncoder gameState.labels)
        , ("allDone", JE.bool gameState.allDone)
        , ("guesses", intBoardEncoder gameState.guesses)
        , ("hints", hintsBoardEncoder gameState.hints)
        , ("flags", flagsEncoder gameState.flags)
        , ("selection", selectionEncoder gameState.selection)
        , ("times", gameStateTimesEncoder gameState.times)
        ]

pairEncoder : (Int, Int) -> Value
pairEncoder pair =
    let (x, y) = pair
    in
        JE.list [JE.int x, JE.int y]

indicesEncoder : List (Int, Int) -> Value
indicesEncoder indices =
    JE.list <| List.map pairEncoder indices

savedModelEncoder : SavedModel4 -> Value
savedModelEncoder model =
    JE.object
        [ ("kind", JE.int model.kind)
        , ("index", JE.int model.index)
        , ("indices", indicesEncoder model.indices)
        , ("gencount", JE.int model.gencount)
        , ("page", pageEncoder model.page)
        , ("gameState", gameStateEncoder model.gameState)
        , ("timestamp", JE.float model.timestamp)
        ]

pageEncoder : Page -> Value
pageEncoder page =
    case page of
        MainPage -> JE.string "main"
        HelpPage -> JE.string "help"
        TacticsPage -> JE.string "tactics"
        CreditsPage -> JE.string "credits"
        IapPage -> JE.string "purchases"

--
-- Decoders
--

xBoardDecoder : Decoder x -> Decoder (Board x)
xBoardDecoder xDecoder =
    let xad = JD.array (JD.array xDecoder)
    in
      JD.map6
          Board.makeWithAll
          (field "rows" JD.int)
          (field "cols" JD.int)
          (field "defaultValue" xDecoder)
          (field "spec" (JD.nullable JD.string))
          (field "index" (JD.nullable JD.int))
          (field "array" xad)

intBoardDecoder : Decoder IntBoard
intBoardDecoder =
    xBoardDecoder JD.int

listToLabels : List Int -> Labels
listToLabels list =
    case list of
      [x, y] -> (x, y)
      _ -> (0, 0)

labelsDecoder : Decoder Labels
labelsDecoder =
    JD.list JD.int
        |> JD.map listToLabels

labelsBoardDecoder : Decoder LabelsBoard
labelsBoardDecoder =
    xBoardDecoder labelsDecoder

hintsDecoder : Decoder Hints
hintsDecoder =
    JD.list JD.int

hintsBoardDecoder : Decoder HintsBoard
hintsBoardDecoder =
    xBoardDecoder hintsDecoder

flagsDecoder : Decoder Flags
flagsDecoder =
    JD.map2
        Flags
        (field "isHintInput" JD.bool)
        (field "showPossibilities" JD.bool)

listToMaybeSelection : Maybe (List Int) -> Maybe Selection
listToMaybeSelection list =
    case list of
      Just [x, y] -> Just (x, y)
      _ -> Nothing

maybeSelectionDecoder : Decoder (Maybe Selection)
maybeSelectionDecoder =
    JD.nullable (JD.list JD.int)
        |> JD.map listToMaybeSelection

pairDecoder : Decoder (Int, Int)
pairDecoder =
    JD.list JD.int
        |> JD.andThen twoListToPairDecoder

twoListToPairDecoder : List Int -> Decoder (Int, Int)
twoListToPairDecoder list =
    case list of
        [a, b] ->
            JD.map2 (,) (JD.succeed a) (JD.succeed b)
        _ ->
            JD.fail <| "Malformed integer pair: " ++ (toString list)

indicesDecoder : Decoder (List (Int, Int))
indicesDecoder =
  JD.list pairDecoder

type alias GameState0 =
    { version : Int --modelVersion
    , board : IntBoard
    , labels : LabelsBoard
    , allDone : Bool
    , guesses : IntBoard
    , hints : HintsBoard
    , flags : Flags
    , selection : Maybe Selection
    }

gameState0Decoder : Decoder GameState0
gameState0Decoder =
    JD.map8
        GameState0
        (field "version" JD.int)
        (field "board" intBoardDecoder)
        (field "labels" labelsBoardDecoder)
        (field "allDone" JD.bool)
        (field "guesses" intBoardDecoder)
        (field "hints" hintsBoardDecoder)
        (field "flags" flagsDecoder)
        (field "selection" maybeSelectionDecoder)

version0 : Int
version0 =
    7

decodeGameState0 : String -> Result String GameState0
decodeGameState0 json =
    case JD.decodeString gameState0Decoder json of
      Err err -> Err err
      Ok gameState ->
          if gameState.version /= version0 then
              Err <|
                  "GameState version mismatch. Expecting: "
                  ++ (toString version0)
                  ++ ", was: " ++ (toString gameState.version)
          else
              Ok gameState

type alias SavedModel0 =
    { version : Int
    , kind : Int
    , index : Int
    , gencount : Int
    , gameState : GameState0
    , time : Time
    }

pageDecoder : Decoder Page
pageDecoder =
    JD.string |> JD.andThen pageHelp

pageHelp : String -> Decoder Page
pageHelp page =
    case page of
        "main" -> JD.succeed MainPage
        "help" -> JD.succeed HelpPage
        "tactics" -> JD.succeed TacticsPage
        "credits" -> JD.succeed CreditsPage
        "purchases" -> JD.succeed IapPage
        _ -> JD.fail <| "Bad page name: " ++ page

savedModel0Decoder : Decoder SavedModel0
savedModel0Decoder =
    JD.map6
        SavedModel0
        (field "version" JD.int)
        (field "kind" JD.int)
        (field "index" JD.int)
        (field "gencount" JD.int)
        (field "gameState" gameState0Decoder)
        (field "time" JD.float)

decodeSavedModel0 : String -> Result String SavedModel0
decodeSavedModel0 json =
    case JD.decodeString savedModel0Decoder json of
      Err err -> Err err
      Ok model ->
          if model.version /= version0 then
              Err <|
                  "SavedModel version mismatch. Expecting: "
                  ++ (toString version0)
                  ++ ", was: " ++ (toString model.version)
          else
              Ok model

--
-- New save/restore code using VersionedJson
--

-- Version 1

type alias GameState1 =
    { board : IntBoard
    , labels : LabelsBoard
    , allDone : Bool
    , guesses : IntBoard
    , hints : HintsBoard
    , flags : Flags
    , selection : Maybe Selection
    }

type alias SavedModel1 =
    { kind : Int
    , index : Int
    , gencount : Int
    , gameState : GameState1
    , time : Time
    }

gameState1Decoder : Decoder GameState1
gameState1Decoder =
    JD.map7
        GameState1
        (field "board" intBoardDecoder)
        (field "labels" labelsBoardDecoder)
        (field "allDone" JD.bool)
        (field "guesses" intBoardDecoder)
        (field "hints" hintsBoardDecoder)
        (field "flags" flagsDecoder)
        (field "selection" maybeSelectionDecoder)

decodeGameState1 : String -> Result String GameState1
decodeGameState1 json =
    JD.decodeString gameState1Decoder json

gameState0To1 : GameState0 -> GameState1
gameState0To1 gameState =
    { board = gameState.board
    , labels = gameState.labels
    , allDone = gameState.allDone
    , guesses = gameState.guesses
    , hints = gameState.hints
    , flags = gameState.flags
    , selection = gameState.selection
    }

gameState0StringTo1 : String -> Result String GameState1
gameState0StringTo1 json =
    case decodeGameState0 json of
      Err s -> Err s
      Ok gameState0 ->
        Ok <| gameState0To1 gameState0

savedModel1Decoder : Decoder SavedModel1
savedModel1Decoder =
    JD.map5
        SavedModel1
        (field "kind" JD.int)
        (field "index" JD.int)
        (field "gencount" JD.int)
        (field "gameState" gameState1Decoder)
        (field "time" JD.float)

decodeSavedModel1 : String -> Result String SavedModel1
decodeSavedModel1 json =
    JD.decodeString savedModel1Decoder json

savedModel0To1 : SavedModel0 -> SavedModel1
savedModel0To1 savedModel =
    { kind = savedModel.kind
    , index = savedModel.index
    , gencount = savedModel.gencount
    , gameState = gameState0To1 savedModel.gameState
    , time = savedModel.time
    }

-- Version 2

type alias GameStateTimes2 =
    { timestamp: Time
    , elapsed: Time
    }

type alias GameState2 =
    { board : IntBoard
    , labels : LabelsBoard
    , allDone : Bool
    , guesses : IntBoard
    , hints : HintsBoard
    , flags : Flags
    , selection : Maybe Selection
    , times: GameStateTimes2
    }

type alias SavedModel2 =
    { kind : Int
    , index : Int
    , gencount : Int
    , gameState : GameState2
    , timestamp : Time
    }

type alias SavedModel3 =
    { kind : Int
    , index : Int
    , gencount : Int
    , gameState : GameState
    , page : Page
    , timestamp : Time
    }

type alias SavedModel4 =
    { kind : Int
    , index : Int
    , indices : List (Int, Int)
    , gencount : Int
    , gameState : GameState
    , page : Page
    , timestamp : Time
    }

gameStateTimes2Decoder : Decoder GameStateTimes2
gameStateTimes2Decoder =
    JD.map2
        GameStateTimes2
        (field "timestamp" JD.float)
        (field "elapsed" JD.float)

gameState2Decoder : Decoder GameState2
gameState2Decoder =
    JD.map8
        GameState2
        (field "board" intBoardDecoder)
        (field "labels" labelsBoardDecoder)
        (field "allDone" JD.bool)
        (field "guesses" intBoardDecoder)
        (field "hints" hintsBoardDecoder)
        (field "flags" flagsDecoder)
        (field "selection" maybeSelectionDecoder)
        (field "times" gameStateTimes2Decoder)

decodeGameState2 : String -> Result String GameState2
decodeGameState2 json =
    JD.decodeString gameState2Decoder json

gameState0To2 : GameState0 -> GameState2
gameState0To2 gameState =
    gameState1To2 <| gameState0To1 gameState

gameState0StringTo2 : String -> Result String GameState2
gameState0StringTo2 json =
    case decodeGameState0 json of
      Err s -> Err s
      Ok gameState0 ->
        Ok <| gameState0To2 gameState0

gameState1To2 : GameState1 -> GameState2
gameState1To2 gameState =
    { board = gameState.board
    , labels = gameState.labels
    , allDone = gameState.allDone
    , guesses = gameState.guesses
    , hints = gameState.hints
    , flags = gameState.flags
    , selection = gameState.selection
    , times = { timestamp = 0, elapsed = 0 }
    }

gameState1StringTo2 : String -> Result String GameState2
gameState1StringTo2 json =
    case decodeGameState1 json of
      Err s -> Err s
      Ok gameState1 ->
        Ok <| gameState1To2 gameState1

savedModel2Decoder : Decoder SavedModel2
savedModel2Decoder =
    JD.map5
        SavedModel2
        (field "kind" JD.int)
        (field "index" JD.int)
        (field "gencount" JD.int)
        (field "gameState" gameState2Decoder)
        (field "timestamp" JD.float)

decodeSavedModel2 : String -> Result String SavedModel2
decodeSavedModel2 json =
    JD.decodeString savedModel2Decoder json

savedModel0To2 : SavedModel0 -> SavedModel2
savedModel0To2 savedModel =
    savedModel1To2 <| savedModel0To1 savedModel

savedModel1To2 : SavedModel1 -> SavedModel2
savedModel1To2 savedModel =
    { kind = savedModel.kind
    , index = savedModel.index
    , gencount = savedModel.gencount
    , gameState = gameState1To2 savedModel.gameState
    , timestamp = savedModel.time
    }

savedModel3Decoder : Decoder SavedModel3
savedModel3Decoder =
    JD.map6
        SavedModel3
        (field "kind" JD.int)
        (field "index" JD.int)
        (field "gencount" JD.int)
        (field "gameState" gameState2Decoder)
        (field "page" pageDecoder)
        (field "timestamp" JD.float)

decodeSavedModel3 : String -> Result String SavedModel3
decodeSavedModel3 json =
    JD.decodeString savedModel3Decoder json

savedModel0To3 : SavedModel0 -> SavedModel3
savedModel0To3 savedModel =
    savedModel2To3 <| savedModel0To2 savedModel

savedModel1To3 : SavedModel1 -> SavedModel3
savedModel1To3 savedModel =
    savedModel2To3 <| savedModel1To2 savedModel

savedModel2To3 : SavedModel2 -> SavedModel3
savedModel2To3 savedModel =
    { kind = savedModel.kind
    , index = savedModel.index
    , gencount = savedModel.gencount
    , gameState = savedModel.gameState
    , page = HelpPage
    , timestamp = savedModel.timestamp
    }

savedModel4Decoder : Decoder SavedModel4
savedModel4Decoder =
    JD.map7
        SavedModel4
        (field "kind" JD.int)
        (field "index" JD.int)
        (field "indices" indicesDecoder)
        (field "gencount" JD.int)
        (field "gameState" gameState2Decoder)
        (field "page" pageDecoder)
        (field "timestamp" JD.float)

decodeSavedModel4 : String -> Result String SavedModel4
decodeSavedModel4 json =
    JD.decodeString savedModel4Decoder json

savedModel0To4 : SavedModel0 -> SavedModel4
savedModel0To4 savedModel =
    savedModel3To4 <| savedModel0To3 savedModel

savedModel1To4 : SavedModel1 -> SavedModel4
savedModel1To4 savedModel =
    savedModel3To4 <| savedModel1To3 savedModel

savedModel2To4 : SavedModel2 -> SavedModel4
savedModel2To4 savedModel =
    savedModel3To4 <| savedModel2To3 savedModel

savedModel3To4 : SavedModel3 -> SavedModel4
savedModel3To4 savedModel =
    { kind = savedModel.kind
    , index = savedModel.index
    , indices = [ (6,1), (8,1), (10,1) ]
    , gencount = savedModel.gencount
    , gameState = savedModel.gameState
    , page = savedModel.page
    , timestamp = savedModel.timestamp
    }

savedModel0StringTo4 : String -> Result String SavedModel4
savedModel0StringTo4 json =
    case decodeSavedModel0 json of
      Err s -> Err s
      Ok savedModel0 ->
        Ok <| savedModel0To4 savedModel0

savedModel1StringTo4 : String -> Result String SavedModel4
savedModel1StringTo4 json =
    case decodeSavedModel1 json of
      Err s -> Err s
      Ok savedModel1 ->
        Ok <| savedModel1To4 savedModel1

savedModel2StringTo4 : String -> Result String SavedModel4
savedModel2StringTo4 json =
    case decodeSavedModel2 json of
      Err s -> Err s
      Ok savedModel2 ->
        Ok <| savedModel2To4 savedModel2

savedModel3StringTo4 : String -> Result String SavedModel4
savedModel3StringTo4 json =
    case decodeSavedModel3 json of
      Err s -> Err s
      Ok savedModel3 ->
        Ok <| savedModel3To4 savedModel3

--
-- Current version encoder and decoder
--

-- Need to add:
--   SavedModel.time -> timestamp
--   GameState.times : GameStateTimes
--   Model.times : ModelTimes

gameStateVersion : Int
gameStateVersion =
    2

savedModelVersion : Int
savedModelVersion =
    4

encodeGameState2 : GameState2 -> String
encodeGameState2 gameState =
    JE.encode 0 <| gameStateEncoder gameState

encodeGameState : GameState -> String
encodeGameState gameState =
    encodeVersionedJson gameStateVersion gameState encodeGameState2

encodeSavedModel4 : SavedModel4 -> String
encodeSavedModel4 model =
    JE.encode 0 <| savedModelEncoder model

encodeSavedModel : SavedModel -> String
encodeSavedModel model =
    encodeVersionedJson savedModelVersion model encodeSavedModel4

gameStateConverterDict : ConverterDict GameState2
gameStateConverterDict =
    Dict.fromList
        [ ( 0, gameState0StringTo2 )
        , ( 1, gameState1StringTo2 )
        , ( 2, decodeGameState2 )
        ]

savedModelConverterDict : ConverterDict SavedModel4
savedModelConverterDict =
    Dict.fromList
        [ ( 0, savedModel0StringTo4 )
        , ( 1, savedModel1StringTo4 )
        , ( 2, savedModel2StringTo4 )
        , ( 3, savedModel3StringTo4 )
        , ( 4, decodeSavedModel4 )
        ]

decodeGameState : String -> Result String GameState
decodeGameState json =
    decodeVersionedJson json gameStateConverterDict

decodeSavedModel : String -> Result String SavedModel
decodeSavedModel json =
    decodeVersionedJson json savedModelConverterDict

---
--- In-App Purchase state
--- Not yet versioned. Will add as necessary.
---

-- Encoders

iapProductEncoder : IapProduct -> Value
iapProductEncoder product =
    JE.object [ ("productId", JE.string product.productId)
              , ("title", JE.string product.title)
              , ("description", JE.string product.description)
              , ("price", JE.string product.price)
              ]

iapPurchaseEncoder : IapPurchase -> Value
iapPurchaseEncoder purchase =
    JE.object [ ("productId", JE.string purchase.productId)
              , ("transactionId", JE.string purchase.transactionId)
              , ("date", JE.int purchase.date)
              ]

iapStateEncoder : IapState -> Value
iapStateEncoder state =
    JE.object [ ("product", iapProductEncoder state.product)
              , ("purchase", iapPurchaseEncoder state.purchase)
              ]

iapStatesEncoder : List IapState -> Value
iapStatesEncoder states =
    JE.list <| List.map iapStateEncoder states

-- decoders

iapProductDecoder : Decoder IapProduct
iapProductDecoder =
    JD.map4 IapProduct
        (JD.field "productId" JD.string)
        (JD.field "title" JD.string)
        (JD.field "description" JD.string)
        (JD.field "price" JD.string)

iapPurchaseDecoder : Decoder IapPurchase
iapPurchaseDecoder =
    JD.map3 IapPurchase
        (JD.field "productId" JD.string)
        (JD.field "transactionId" JD.string)
        (JD.field "date" JD.int)

iapStateDecoder : Decoder IapState
iapStateDecoder =
    JD.map2 IapState
        (JD.field "product" iapProductDecoder)
        (JD.field "purchase" iapPurchaseDecoder)

iapStatesDecoder : Decoder (List IapState)
iapStatesDecoder =
    JD.list iapStateDecoder

-- API

encodeIapStates : List IapState -> String
encodeIapStates state =
    JE.encode 0 <| iapStatesEncoder state

decodeIapStates : String -> Result String (List IapState)
decodeIapStates json =
    JD.decodeString iapStatesDecoder json
