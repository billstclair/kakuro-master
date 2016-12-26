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
                             , decodeGameState, decodeSavedModel)

import SharedTypes exposing ( Labels, Hints, Flags, Selection
                            , IntBoard, LabelsBoard, HintsBoard
                            , GameStateTimes
                            , GameState, Page(..), SavedModel
                            )
import Board exposing (Board)

import VersionedJson exposing ( ConverterDict
                              , encodeVersionedJson, decodeVersionedJson)

import Json.Decode as JD exposing (field, Decoder)
import Json.Encode as JE exposing (Value)
import Time exposing (Time)
import Dict
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

savedModelEncoder : SavedModel3 -> Value
savedModelEncoder model =
    JE.object
        [ ("kind", JE.int model.kind)
        , ("index", JE.int model.index)
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

savedModel0StringTo1 : String -> Result String SavedModel1
savedModel0StringTo1 json =
    case decodeSavedModel0 json of
      Err s -> Err s
      Ok savedModel0 ->
        Ok <| savedModel0To1 savedModel0

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

savedModel0StringTo2 : String -> Result String SavedModel2
savedModel0StringTo2 json =
    case decodeSavedModel0 json of
      Err s -> Err s
      Ok savedModel0 ->
        Ok <| savedModel0To2 savedModel0

savedModel1StringTo2 : String -> Result String SavedModel2
savedModel1StringTo2 json =
    case decodeSavedModel1 json of
      Err s -> Err s
      Ok savedModel1 ->
        Ok <| savedModel1To2 savedModel1

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

savedModel0StringTo3 : String -> Result String SavedModel3
savedModel0StringTo3 json =
    case decodeSavedModel0 json of
      Err s -> Err s
      Ok savedModel0 ->
        Ok <| savedModel0To3 savedModel0

savedModel1StringTo3 : String -> Result String SavedModel3
savedModel1StringTo3 json =
    case decodeSavedModel1 json of
      Err s -> Err s
      Ok savedModel1 ->
        Ok <| savedModel1To3 savedModel1

savedModel2StringTo3 : String -> Result String SavedModel3
savedModel2StringTo3 json =
    case decodeSavedModel2 json of
      Err s -> Err s
      Ok savedModel2 ->
        Ok <| savedModel2To3 savedModel2

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
    3

encodeGameState2 : GameState2 -> String
encodeGameState2 gameState =
    JE.encode 0 <| gameStateEncoder gameState

encodeGameState : GameState -> String
encodeGameState gameState =
    encodeVersionedJson gameStateVersion gameState encodeGameState2

encodeSavedModel3 : SavedModel3 -> String
encodeSavedModel3 model =
    JE.encode 0 <| savedModelEncoder model

encodeSavedModel : SavedModel -> String
encodeSavedModel model =
    encodeVersionedJson savedModelVersion model encodeSavedModel3

gameStateConverterDict : ConverterDict GameState2
gameStateConverterDict =
    Dict.fromList
        [ ( 0, gameState0StringTo2 )
        , ( 1, gameState1StringTo2 )
        , ( 2, decodeGameState2 )
        ]

savedModelConverterDict : ConverterDict SavedModel3
savedModelConverterDict =
    Dict.fromList
        [ ( 0, savedModel0StringTo3 )
        , ( 1, savedModel1StringTo3 )
        , ( 2, savedModel2StringTo3 )
        , ( 3, decodeSavedModel3 )
        ]

decodeGameState : String -> Result String GameState
decodeGameState json =
    decodeVersionedJson json gameStateConverterDict

decodeSavedModel : String -> Result String SavedModel
decodeSavedModel json =
    decodeVersionedJson json savedModelConverterDict
