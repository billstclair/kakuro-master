module TestGenerate exposing (main)

import Board exposing (Board)
import BoardSize
import Browser
import Dict exposing (Dict)
import Generate exposing (cellChoices, generate, randomChoice)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import List.Extra as LE
import PuzzleDB
import RenderBoard
import SharedTypes
    exposing
        ( Flags
        , GameState
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
    , step : Maybe (Mdl -> Mdl)
    , showHints : Bool
    , row : Int
    , col : Int
    }


type Mdl
    = Mdl Model


initialModel : Model
initialModel =
    { kakuroModel = initialKakuroModel
    , step = Nothing
    , showHints = True
    , row = 0
    , col = 0
    }


type Msg
    = Noop
    | Tick Posix
    | StepCellChoices
    | GenerateChoices
    | ToggleHints


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Tick posix ->
            case model.step of
                Nothing ->
                    ( model, Cmd.none )

                Just stepper ->
                    case stepper <| Mdl model of
                        Mdl model2 ->
                            ( model2, Cmd.none )

        StepCellChoices ->
            ( { model
                | row = 0
                , col = 0
                , step = Just stepCellChoices
              }
                |> clearGameState
            , Cmd.none
            )

        GenerateChoices ->
            let
                newModel =
                    clearGameState model

                newHints =
                    Generate.generateChoices <| modelBoard newModel
            in
            ( doGameState (\gs -> { gs | hints = newHints }) newModel
            , Cmd.none
            )

        ToggleHints ->
            let
                showHints =
                    not model.showHints

                hints =
                    modelHints model
            in
            ( { model | showHints = showHints }
                |> clearGameState
                |> doHints (\_ -> hints)
                |> (if showHints then
                        identity

                    else
                        guessRight
                   )
            , Cmd.none
            )


doKakuroModel : (SharedTypes.Model -> SharedTypes.Model) -> Model -> Model
doKakuroModel f model =
    { model
        | kakuroModel = f model.kakuroModel
    }


doFlags : (Flags -> Flags) -> Model -> Model
doFlags f model =
    doGameState
        (\gs -> { gs | flags = f gs.flags })
        model


doGameState : (GameState -> GameState) -> Model -> Model
doGameState f model =
    doKakuroModel (\km -> { km | gameState = f km.gameState }) model


doBoard : (IntBoard -> IntBoard) -> Model -> Model
doBoard f model =
    doGameState (\gs -> { gs | board = f gs.board })
        model


doGuesses : (IntBoard -> IntBoard) -> Model -> Model
doGuesses f model =
    doGameState (\gs -> { gs | guesses = f gs.guesses })
        model


doHints : (HintsBoard -> HintsBoard) -> Model -> Model
doHints f model =
    doGameState (\gs -> { gs | hints = f gs.hints })
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


boardGet : Int -> Int -> Model -> Int
boardGet row col model =
    let
        board =
            modelBoard model
    in
    Board.get row col board


boardSet : Int -> Int -> Int -> Model -> Model
boardSet row col val model =
    doBoard (Board.set row col val) model


guessesSet : Int -> Int -> Int -> Model -> Model
guessesSet row col val model =
    doGuesses (Board.set row col val) model


hintsSet : Int -> Int -> Hints -> Model -> Model
hintsSet row col val model =
    doHints (Board.set row col val) model


modelHints : Model -> HintsBoard
modelHints model =
    model.kakuroModel.gameState.hints


modelBoard : Model -> IntBoard
modelBoard model =
    model.kakuroModel.gameState.board


modelGameState : Model -> GameState
modelGameState model =
    model.kakuroModel.gameState


fixChoicesStep : Mdl -> Mdl
fixChoicesStep (Mdl model) =
    let
        newModel =
            { model | step = Nothing }

        newHints =
            Generate.fixChoicesForSums (modelBoard newModel)
                (modelHints newModel)
    in
    doGameState (\gameState -> { gameState | hints = newHints }) newModel
        |> Mdl


stepCellChoices : Mdl -> Mdl
stepCellChoices (Mdl model) =
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
        choices =
            Generate.cellChoices row col <| modelBoard model

        ( ch, zeroOk ) =
            if List.member 0 choices then
                ( LE.remove 0 choices, True )

            else
                ( choices, False )

        newModel =
            hintsSet row col ch model

        ( nextRow, nextCol, nextStep ) =
            if col >= cols - 1 then
                if row >= rows - 1 then
                    ( 0, 0, Just fixChoicesStep )

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


b : String -> Html msg
b s =
    Html.span [ style "font-weight" "bold" ]
        [ text s ]


br : Html msg
br =
    Html.br []
        []


view : Model -> Html Msg
view model =
    div [ style "margin" "2em" ]
        [ h2 "TestGenerate"
        , p []
            [ button
                [ onClick StepCellChoices
                , disabled <| model.step /= Nothing
                ]
                [ text "StepCellChoices" ]
            , text " "
            , button
                [ onClick GenerateChoices
                , disabled <| model.step /= Nothing
                ]
                [ text "GenerateChoices" ]
            , br
            , button [ onClick ToggleHints ]
                [ text <|
                    if model.showHints then
                        "Show board"

                    else
                        "Show hints"
                ]
            ]
        , p []
            [ b "row: "
            , text <| String.fromInt <| model.row + 1
            , text " "
            , b "col: "
            , text <| String.fromInt <| model.col + 1
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
                { flags
                    | showPossibilities = False
                    , isHintInput = True
                }
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


guessRight : Model -> Model
guessRight model =
    let
        board =
            modelBoard model

        rows =
            board.rows

        cols =
            board.cols

        rowRange =
            List.range 0 (rows - 1)

        colRange =
            List.range 0 (cols - 1)

        eachRow row m =
            List.foldl (eachCol row) m colRange

        eachCol row col m =
            guessesSet row col (Board.get row col board) m
    in
    List.foldl eachRow model rowRange


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
        [ Time.every 200 Tick
        ]
