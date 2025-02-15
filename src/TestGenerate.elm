module TestGenerate exposing (main)

import Board exposing (Board)
import BoardSize
import Browser
import Dict exposing (Dict)
import Generate exposing (columnChoices, generate, randomChoice)
import Html exposing (Html, button, div, p, text)
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
import Time exposing (Posix)


type alias Model =
    { kakuroModel : SharedTypes.Model
    , step : Mdl -> Mdl
    , row : Int
    , col : Int
    }


type Mdl
    = Mdl Model


initialModel : Model
initialModel =
    { kakuroModel = initialKakuroModel
    , step = identity
    , row = 1
    , col = 1
    }


type Msg
    = Noop
    | Tick Posix
    | StepColumnChoices


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Tick posix ->
            let
                mdl =
                    model.step <| Mdl model
            in
            case mdl of
                Mdl model2 ->
                    ( model2, Cmd.none )

        StepColumnChoices ->
            ( { model
                | row = 0
                , col = 0
                , step = stepColumnChoices
              }
                |> clearGameState
            , Cmd.none
            )


doGameState : (GameState -> GameState) -> Model -> Model
doGameState f model =
    let
        km =
            model.kakuroModel
    in
    { model
        | kakuroModel =
            { km | gameState = f km.gameState }
    }


doBoard : (IntBoard -> IntBoard) -> Model -> Model
doBoard f model =
    doGameState (\gs -> { gs | board = f gs.board })
        model


doGuesses : (IntBoard -> IntBoard) -> Model -> Model
doGuesses f model =
    doGameState (\gs -> { gs | guesses = f gs.guesses })
        model


clearGameState : Model -> Model
clearGameState model =
    model
        |> doGameState
            (\gs ->
                let
                    board =
                        gs.board

                    rows =
                        board.rows

                    cols =
                        board.cols
                in
                { gs
                    | guesses =
                        Board.make rows cols board.defaultValue
                    , hints =
                        Board.make rows cols []
                }
            )


boardSet : Int -> Int -> Int -> Model -> Model
boardSet row col val model =
    doBoard (\board -> Board.set row col val board) model


guessesSet : Int -> Int -> Int -> Model -> Model
guessesSet row col val model =
    doGuesses (\board -> Board.set row col val board) model


modelBoard : Model -> IntBoard
modelBoard model =
    model.kakuroModel.gameState.board


stepColumnChoices : Mdl -> Mdl
stepColumnChoices (Mdl model) =
    let
        board =
            modelBoard model

        rows =
            board.rows

        cols =
            board.cols

        row =
            model.row

        col =
            model.col
    in
    let
        newModel =
            guessesSet row col ((row + 1) * 10 + col + 1) model

        ( nextRow, nextCol, nextStep ) =
            if col >= cols - 1 then
                if row >= rows - 1 then
                    ( 0, 0, identity )

                else
                    ( row + 1, 0, newModel.step )

            else
                ( row, col + 1, newModel.step )
    in
    { newModel
        | row = nextRow
        , col = nextCol
        , step = nextStep
    }
        |> Mdl


h2 : String -> Html msg
h2 s =
    Html.h2 [] [ text s ]


view : Model -> Html Msg
view model =
    div [ style "margin" "2em" ]
        [ h2 "TestGenerate"
        , p []
            [ button [ onClick StepColumnChoices ]
                [ text "StepColumnChoices" ]
            ]
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

        flags =
            state.flags
    in
    { kind = initialKind
    , index = idx
    , indices = [ ( 6, 1 ), ( 8, 1 ), ( 10, 1 ) ]
    , gencount = 0
    , page = HelpPage
    , gameState =
        { state
            | flags =
                { flags | showPossibilities = False }
        }
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
        |> (\model ->
                let
                    boardSizes =
                        BoardSize.computeBoardSizes model
                in
                { model
                    | boardSizes =
                        Just
                            { boardSizes
                                | cellFontSize =
                                    toFloat boardSizes.cellFontSize * 3 / 4 |> round
                            }
                }
           )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 125 Tick
        ]
