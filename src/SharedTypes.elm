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

module SharedTypes exposing ( SavedModel, ModelTimes, Model
                            , emptyModelTimes, emptyGameStateTimes
                            , modelToSavedModel, savedModelToModel
                            , BoardSizes
                            , Msg, Msg(..)
                            , Selection, GameStateTimes, GameState, Flags, Page(..)
                            , IntBoard, BClassMatrix, BClassBoard
                            , Labels, LabelsBoard, Hints, HintsBoard
                            , HelpModelDict, MaybeHelpModelDict(..)
                            )

import SimpleMatrix exposing (Matrix)
import Styles.Board exposing (BClass)
import Board exposing (Board)
import PuzzleDB
import Random
import Time exposing (Time, second)
import Keyboard
import Window
import Dict exposing (Dict)

type Page
    = MainPage
    | HelpPage
    | TacticsPage

-- This gets saved in the browser database.

type alias SavedModel =
    { kind : Int
    , index : Int
    , gencount : Int
    , gameState : GameState
    , page : Page
    , timestamp : Time
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

type alias ModelTimes =
    { timestamp : Time
    , lastPersist : Maybe Time
    }

emptyModelTimes : ModelTimes
emptyModelTimes =
    ModelTimes 0 Nothing

type alias Model =
    { -- on disk. Copied to and from SavedModel instance.
      kind : Int
    , index : Int
    , gencount : Int
    , page : Page
    , gameState : GameState
    -- in-memory only
    , times : ModelTimes
    , windowSize : Maybe Window.Size
    , boardSizes : Maybe BoardSizes
    , seed : Maybe Random.Seed
    , awaitingCommand : Maybe String
    , message : Maybe String
    , shifted : Bool
    , helpModelDict : MaybeHelpModelDict
    , isCordova : Bool
    }

modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { kind = model.kind
    , index = model.index
    , gencount = model.gencount
    , gameState = model.gameState
    , page = model.page
    , timestamp = model.times.timestamp
    }

savedModelToModel : SavedModel -> Model
savedModelToModel savedModel =
    { kind = savedModel.kind
    , index = savedModel.index
    , gencount = savedModel.gencount
    , gameState = savedModel.gameState
    , page = savedModel.page
    , times = emptyModelTimes
    , boardSizes = Nothing
    , windowSize = Nothing
    , seed = Nothing
    , awaitingCommand = Nothing
    , message = Nothing
    , shifted = False
    , helpModelDict = Nicht
    , isCordova = False
    }

type Msg
    = Generate Int
    | Restart
    | ChangeKind Int
    | Tick Time
    | Seed Time
    | ClickCell String
    | PressKey Keyboard.KeyCode
    | DownKey Keyboard.KeyCode
    | UpKey Keyboard.KeyCode
    | ToggleHintInput
    | ToggleShowPossibilities
    | ReceiveGame (Maybe String)
    | AnswerConfirmed String Bool
    | MultiAnswerConfirmed String Int
    | PromptAnswerConfirmed String String
    | NewBoardIndex String
    | WindowSize Window.Size
    | ShowPage Page
    | GetBoardIndex
    | Nop

type alias IntBoard =
    Board Int

type alias Labels =
    ( Int, Int )

type alias LabelsBoard =
    Board Labels

type alias Hints =
    List Int

type alias HintsBoard =
    Board Hints

type alias Selection =
    ( Int, Int )

type alias BClassMatrix =
    Matrix (Maybe BClass)

type alias BClassBoard =
    Board (Maybe BClass)

type alias Flags =
    { isHintInput : Bool
    , showPossibilities : Bool
    }

type alias GameStateTimes =
    { timestamp: Time
    , elapsed: Time
    }

emptyGameStateTimes : GameStateTimes
emptyGameStateTimes =
    GameStateTimes 0 0

type alias GameState =
    { board : IntBoard
    , labels : LabelsBoard
    , allDone : Bool
    , guesses : IntBoard
    , hints : HintsBoard
    , flags : Flags
    , selection : Maybe Selection
    , times: GameStateTimes
    }

type alias HelpModelDict =
    Dict String Model

type MaybeHelpModelDict
    = Nicht
    | Javole HelpModelDict
