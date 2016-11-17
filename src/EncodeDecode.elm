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

module EncodeDecode exposing (decodeGameState0, decodeSavedModel0)

import SharedTypes exposing ( Labels, Hints, Flags, Selection
                            , IntBoard, LabelsBoard, HintsBoard
                            )
import Board exposing (Board)

import Json.Decode as JD exposing (field, Decoder)
import Json.Encode as JE
import Time exposing (Time)

type alias Model =
    Int

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

gameStateDecoder : Decoder GameState0
gameStateDecoder =
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
    case JD.decodeString gameStateDecoder json of
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

savedModelDecoder : Decoder SavedModel0
savedModelDecoder =
    JD.map6
        SavedModel0
        (field "version" JD.int)
        (field "kind" JD.int)
        (field "index" JD.int)
        (field "gencount" JD.int)
        (field "gameState" gameStateDecoder)
        (field "time" JD.float)

decodeSavedModel0 : String -> Result String SavedModel0
decodeSavedModel0 json =
    case JD.decodeString savedModelDecoder json of
      Err err -> Err err
      Ok model ->
          if model.version /= version0 then
              Err <|
                  "SavedModel version mismatch. Expecting: "
                  ++ (toString version0)
                  ++ ", was: " ++ (toString model.version)
          else
              Ok model
