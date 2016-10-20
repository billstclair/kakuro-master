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

module SharedTypes exposing ( Model, modelVersion
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

-- The JavaScript code in index.html needs to know this version
-- number. If it reads a model with a version it doesn't recognize,
-- it will act as if there's no saved state, to prevent a
-- run-time error unpacking the save data into Elm data structures.
-- The version needs to be bumped any time ANY state reachable
-- from the model is changed, in shape or type.
modelVersion : Int
modelVersion = 1

type alias Model =
      { version: Int
      , kind : Int
      , index : Int
      , gencount : Int
      , gameState : GameState
--      , seed : Maybe Random.Seed
      , time : Time
      }

type Msg
  = Generate Int
  | ChangeKind Int
  | Tick Time
--  | Seed Time
  | ClickCell String
  | PressKey Keyboard.KeyCode
  | ToggleHintInput
  | ToggleShowPossibilities
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
  { board : IntBoard
  , labels : LabelsBoard
  , allDone : Bool
  , guesses : IntBoard
  , hints : HintsBoard
  , flags : Flags
  , selection : Maybe Selection
  }
