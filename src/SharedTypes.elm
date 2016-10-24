----------------------------------------------------------------------
--
-- SharedTypes.elm
-- Shared types
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module SharedTypes exposing ( SavedModel, modelVersion, Model
                            , modelToSavedModel, savedModelToModel
                            , BoardSizes
                            , Msg, Msg (..)
                            , Selection
                            , GameState
                            , Flags
                            , IntBoard
                            , BClassMatrix
                            , BClassBoard
                            , Labels, LabelsBoard
                            , Hints, HintsBoard 
                            )

import SimpleMatrix exposing (Matrix)
import Styles.Board exposing (BClass) 
import Board exposing(Board)
import PuzzleDB

import Random
import Time exposing (Time, second)
import Keyboard
import Window

-- The JavaScript code in index.html needs to know this version
-- number. If it reads a model with a version it doesn't recognize,
-- it will act as if there's no saved state, to prevent a
-- run-time error unpacking the save data into Elm data structures.
-- The version needs to be bumped any time ANY state reachable
-- from the model is changed, in shape or type.
modelVersion : Int
modelVersion = 7

-- This gets saved in the browser database.
-- Changing it currently causes all saved state to be lost.
-- Fix that eventually.
type alias SavedModel =
  { version: Int
  , kind : Int
  , index : Int
  , gencount : Int
  , gameState : GameState
  , time : Time
  }

type alias BoardSizes =
  { boardSize : Int
  , cellSize : Int
  , cellFontSize : Int
  , labelFontSize : Int
  , hintFontSize : Int
  , keypadSize : Int
  , keypadFontSize : Int
  }

type alias Model =
  { -- on disk. Copied to and from SavedModel instance.
    kind : Int
  , index : Int
  , gencount : Int
  , gameState : GameState
  , time : Time
  -- in-memory only
  , windowSize : Maybe Window.Size
  , boardSizes : Maybe BoardSizes
  , seed : Maybe Random.Seed
  , awaitingCommand : Maybe String
  , message : Maybe String
  }

modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
  { version = modelVersion
  , kind = model.kind
  , index = model.index
  , gencount = model.gencount
  , gameState = model.gameState
  , time = model.time
  }

savedModelToModel : SavedModel -> Model
savedModelToModel savedModel =
  { kind = savedModel.kind
  , index = savedModel.index
  , gencount = savedModel.gencount
  , gameState = savedModel.gameState
  , time = savedModel.time
  , boardSizes = Nothing
  , windowSize = Nothing
  , seed = Nothing
  , awaitingCommand = Nothing
  , message = Nothing           
  }

type Msg
  = Generate Int
  | Restart
  | ChangeKind Int
  | Tick Time
  | Seed Time
  | ClickCell String
  | PressKey Keyboard.KeyCode
  | ToggleHintInput
  | ToggleShowPossibilities
  | ReceiveGame (Maybe GameState)
  | AnswerConfirmed String Bool
  | WindowSize Window.Size
  | Nop

type alias IntBoard =
  Board Int

type alias Labels =
  (Int, Int)

type alias LabelsBoard =
  Board Labels

type alias Hints =
  List Int

type alias HintsBoard =
  Board Hints

type alias Selection =
  (Int, Int)

type alias BClassMatrix =
  Matrix (Maybe BClass)

type alias BClassBoard =
  Board (Maybe BClass)

type alias Flags =
  { isHintInput : Bool
  , showPossibilities : Bool
  }

type alias GameState =
  { version: Int                --modelVersion
  , board : IntBoard
  , labels : LabelsBoard
  , allDone : Bool
  , guesses : IntBoard
  , hints : HintsBoard
  , flags : Flags
  , selection : Maybe Selection
  }
