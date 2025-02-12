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


module SharedTypes exposing
    ( BClassBoard
    , BClassMatrix
    , BoardSizes
    , ExploreState
    , Flags
    , GameState
    , HelpModelDict
    , Hints
    , HintsBoard
    , IntBoard
    , Labels
    , LabelsBoard
    , MaybeHelpModelDict(..)
    , Model
    , Msg(..)
    , Page(..)
    , Platform(..)
    , SavedModel
    , Selection
    , WindowSize
    , modelToSavedModel
    , savedModelToModel
    )

import Board exposing (Board)
import Browser.Dom as Dom exposing (Viewport)
import Dict exposing (Dict)
import PuzzleDB
import Random
import SimpleMatrix exposing (Matrix)
import Styles.Board exposing (BClass)
import Time exposing (Posix)


type Page
    = MainPage
    | HelpPage
    | TacticsPage
    | CreditsPage


type Platform
    = WebPlatform
    | IosPlatform
    | AndroidPlatform



-- This gets saved in the browser database.


type alias SavedModel =
    { kind : Int
    , index : Int
    , indices : List ( Int, Int )
    , gencount : Int
    , gameState : GameState
    , page : Page
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


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Model =
    { -- on disk. Copied to and from SavedModel instance.
      kind : Int
    , index : Int
    , indices : List ( Int, Int )
    , gencount : Int
    , page : Page
    , gameState : GameState

    -- in-memory only
    , windowSize : Maybe WindowSize
    , boardSizes : Maybe BoardSizes
    , seed : Maybe Random.Seed
    , awaitingCommand : Maybe String
    , message : Maybe String
    , shifted : Bool
    , showStarMenu : Bool
    , helpModelDict : MaybeHelpModelDict
    , platform : Platform
    , properties : Dict String String --raw properties at startup
    , deviceReady : Bool
    }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { kind = model.kind
    , index = model.index
    , indices = model.indices
    , gencount = model.gencount
    , gameState = model.gameState
    , page = model.page
    }


savedModelToModel : SavedModel -> Model
savedModelToModel savedModel =
    { kind = savedModel.kind
    , index = savedModel.index
    , indices = savedModel.indices
    , gencount = savedModel.gencount
    , gameState = savedModel.gameState
    , page = savedModel.page
    , boardSizes = Nothing
    , windowSize = Nothing
    , seed = Nothing
    , awaitingCommand = Nothing
    , message = Nothing
    , shifted = False
    , showStarMenu = False
    , helpModelDict = Nicht
    , platform = WebPlatform
    , properties = Dict.fromList []
    , deviceReady = False
    }


type Msg
    = Generate Int
    | Restart
    | ChangeKind Int
    | Seed Posix
    | ClickCell String
    | PressKey String
    | DownKey Bool String
    | UpKey String
    | ToggleHintInput
    | ToggleShowPossibilities
    | ToggleKeyClick
    | OpenStarMenu
    | StartExploration
    | KeepExploration
    | DiscardExploration
    | CloseStarMenu
    | ReceiveGame (Maybe String)
    | AnswerConfirmed String Bool
    | MultiAnswerConfirmed String Int
    | PromptAnswerConfirmed String String
    | NewBoardIndex String
    | SetWindowSize Viewport
    | UpdateWindowSize Int Int
    | ShowPage Page
    | GetBoardIndex
    | DeviceReady String
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
    , firstGuess : Int
    , keyClickSound : Bool
    }


type alias ExploreState =
    { savedBoard : IntBoard
    , savedHints : HintsBoard
    , guesses : IntBoard
    , firstGuess : Int
    , firstGuessSelection : Maybe Selection
    }


type alias GameState =
    { board : IntBoard
    , labels : LabelsBoard
    , guesses : IntBoard
    , hints : HintsBoard
    , flags : Flags
    , selection : Maybe Selection
    , exploreState : Maybe ExploreState
    }


type alias HelpModelDict =
    Dict String Model


type MaybeHelpModelDict
    = Nicht
    | Javole HelpModelDict
