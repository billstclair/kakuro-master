module TestGenerate exposing (main)

import Board exposing (Board)
import Browser
import Dict exposing (Dict)
import Generate exposing (columnChoices, generate, randomChoice)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import PuzzleDB
import RenderBoard
import SharedTypes
    exposing
        ( GameState
        , Hints
        , HintsBoard
        , IntBoard
        , MaybeHelpModelDict(..)
        , Page(..)
        , Platform(..)
        )


type alias Model =
    { kakuroModel : SharedTypes.Model
    }


initialModel : Model
initialModel =
    { kakuroModel = initialKakuroModel
    }


type Msg
    = Noop


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model


h2 : String -> Html msg
h2 s =
    Html.h2 [] [ text s ]


view : Model -> Html Msg
view model =
    div [ style "margin" "2em" ]
        [ h2 "TestGenerate"
        , Html.map (\_ -> Noop) <| RenderBoard.render model.kakuroModel
        ]


initialKind : Int
initialKind =
    6


realBoardIndex : IntBoard -> Int
realBoardIndex board =
    case board.index of
        Nothing ->
            0

        Just index ->
            index


initialKakuroModel : SharedTypes.Model
initialKakuroModel =
    let
        board =
            PuzzleDB.getBoardOfKind initialKind 1

        state =
            RenderBoard.makeGameState board

        idx =
            realBoardIndex board
    in
    { kind = initialKind
    , index = idx
    , indices = [ ( 6, 1 ), ( 8, 1 ), ( 10, 1 ) ]
    , gencount = 0
    , page = HelpPage
    , gameState = state
    , windowSize = Nothing
    , boardSizes = Nothing
    , seed = Nothing
    , awaitingCommand = Nothing
    , message = Nothing
    , shifted = False
    , showStarMenu = False
    , helpModelDict = Nicht
    , platform = WebPlatform
    , properties = Dict.fromList []
    , deviceReady = False
    , savedModel = Nothing
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
