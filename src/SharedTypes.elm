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
                            , Selection, GameStateTimes, GameState, ExploreState
                            , Flags, Page(..)
                            , IntBoard, BClassMatrix, BClassBoard
                            , Labels, LabelsBoard, Hints, HintsBoard
                            , HelpModelDict, MaybeHelpModelDict(..)
                            , IapProduct, IapPurchase, IapState
                            , Platform(..)
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
    | CreditsPage
    | IapPage
    | AdvertisePage

type Platform
    = WebPlatform
    | IosPlatform
    | AndroidPlatform

-- This gets saved in the browser database.

type alias SavedModel =
    { kind : Int
    , index : Int
    , indices : List (Int, Int)
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
    , unlockDate : String
    , unlockHash : String
    }

emptyModelTimes : ModelTimes
emptyModelTimes =
    ModelTimes 0 Nothing "161231" "316966da"

type alias Model =
    { -- on disk. Copied to and from SavedModel instance.
      kind : Int
    , index : Int
    , indices : List (Int, Int)
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
    , showStarMenu : Bool
    , helpModelDict : MaybeHelpModelDict
    , platform : Platform
    , properties : Dict String String         --raw properties at startup
    , deviceReady : Bool
    , iapState : Maybe (Dict String IapState) --from storage
    , iapProducts: Maybe (Maybe (List IapProduct), Maybe String) --from iap call
    }

modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { kind = model.kind
    , index = model.index
    , indices = model.indices
    , gencount = model.gencount
    , gameState = model.gameState
    , page = model.page
    , timestamp = model.times.timestamp
    }

savedModelToModel : SavedModel -> Model
savedModelToModel savedModel =
    { kind = savedModel.kind
    , index = savedModel.index
    , indices = savedModel.indices
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
    , showStarMenu = False
    , helpModelDict = Nicht
    , platform = WebPlatform
    , properties = Dict.fromList []
    , deviceReady = False
    , iapState = Nothing
    , iapProducts = Nothing
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
    | IapProducts (Maybe (List IapProduct), Maybe String)
    | ReloadIapProducts
    | RestoreIapPurchases
    | IapPurchases (Maybe (List IapPurchase), Maybe String)
    | NewBoardIndex String
    | WindowSize Window.Size
    | ShowPage Page
    | GetBoardIndex
    | DeviceReady String
    | IapBuy String
    | IapBuyResponse (String, Maybe String, Maybe String)
    | InvokeSpecHashReceiver (String -> String -> Model -> Model) String String
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

type alias GameStateTimes =
    { timestamp: Time
    , elapsed: Time
    }

emptyGameStateTimes : GameStateTimes
emptyGameStateTimes =
    GameStateTimes 0 0

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
    , times: GameStateTimes
    }

type alias HelpModelDict =
    Dict String Model

type MaybeHelpModelDict
    = Nicht
    | Javole HelpModelDict

type alias IapProduct =
    { productId : String
    , title : String
    , description : String
    , price : String
    }

type alias IapPurchase =
    { productId : String
    , transactionId : String
    , date : Time
    }

-- A list of these is stored as JSON on the "kakuro-iap" property.
-- There is currently only one, with a productId of "puzzles2".
-- See EncodeDecode.
type alias IapState =
    { product : IapProduct
    , purchase : IapPurchase
    }
