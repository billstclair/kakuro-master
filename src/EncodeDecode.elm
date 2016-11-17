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

module EncodeDecode exposing ( GameState1, SavedModel1
                             , encodeGameState, encodeSavedModel
                             , decodeGameState, decodeSavedModel)

import SharedTypes exposing ( Labels, Hints, Flags, Selection
                            , IntBoard, LabelsBoard, HintsBoard
                            , GameState, SavedModel
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

gameStateEncoder : GameState1 -> Value
gameStateEncoder gameState =
    JE.object
        [ ("board", intBoardEncoder gameState.board)
        , ("labels", labelsBoardEncoder gameState.labels)
        , ("allDone", JE.bool gameState.allDone)
        , ("guesses", intBoardEncoder gameState.guesses)
        , ("hints", hintsBoardEncoder gameState.hints)
        , ("flags", flagsEncoder gameState.flags)
        , ("selection", selectionEncoder gameState.selection)
        ]

savedModelEncoder : SavedModel1 -> Value
savedModelEncoder model =
    JE.object
        [ ("kind", JE.int model.kind)
        , ("index", JE.int model.index)
        , ("gencount", JE.int model.gencount)
        , ("gameState", gameStateEncoder model.gameState)
        , ("time", JE.float model.time)
        ]

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

--
-- version1 is the same as version0,
-- except it's saved as { version: Int, value: String }
--

-- Change to just "= GameState"
type alias GameState1 =
    { board : IntBoard
    , labels : LabelsBoard
    , allDone : Bool
    , guesses : IntBoard
    , hints : HintsBoard
    , flags : Flags
    , selection : Maybe Selection
    }

-- Change to "= SavedModel"
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

--
-- Current version encoder and decoder
--

gameStateVersion : Int
gameStateVersion =
    1

savedModelVersion : Int
savedModelVersion =
    1

encodeGameState1 : GameState1 -> String
encodeGameState1 gameState =
    JE.encode 0 <| gameStateEncoder gameState

encodeGameState : GameState1 -> String
encodeGameState gameState =
    encodeVersionedJson gameStateVersion gameState encodeGameState1

encodeSavedModel1 : SavedModel1 -> String
encodeSavedModel1 model =
    JE.encode 0 <| savedModelEncoder model

encodeSavedModel : SavedModel1 -> String
encodeSavedModel model =
    encodeVersionedJson savedModelVersion model encodeSavedModel1

gameStateConverterDict : ConverterDict GameState1
gameStateConverterDict =
    Dict.fromList
        [ ( 0, gameState0StringTo1 )
        , ( 1, decodeGameState1 )
        ]

savedModelConverterDict : ConverterDict SavedModel1
savedModelConverterDict =
    Dict.fromList
        [ ( 0, savedModel0StringTo1 )
        , ( 1, decodeSavedModel1 )
        ]

decodeGameState : String -> Result String GameState1
decodeGameState json =
    decodeVersionedJson json gameStateConverterDict

decodeSavedModel : String -> Result String SavedModel1
decodeSavedModel json =
    decodeVersionedJson json savedModelConverterDict
