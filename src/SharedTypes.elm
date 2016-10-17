----------------------------------------------------------------------
--
-- SharedTypes.elm
-- kakuro-master.com main screen
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module SharedTypes exposing ( Model
                            , Msg, Msg (..)
                            , Selection
                            , GameState
                            , IntBoard
                            , Labels, LabelsBoard
                            , Hints, HintsBoard 
                            )

import Board exposing(Board)
import PuzzleDB

import Random
import Time exposing (Time, second)
import Keyboard

type alias Model =
      { kind : Int
      , index : Int
      , gencount : Int
      , gameState : GameState
      , seed : Maybe Random.Seed
      , time : Time
      }

type Msg
  = Generate
  | Tick Time
  | Seed Time
  | ClickCell String
  | PressKey Keyboard.KeyCode
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

type alias GameState =
  { board : IntBoard
  , labels: LabelsBoard
  , guesses : IntBoard
  , hints : HintsBoard
  , selection : Maybe Selection
  }