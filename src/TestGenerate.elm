module TestGenerate exposing (main)

import Board exposing (Board)
import BoardSize
import Browser
import Dict exposing (Dict)
import Generate exposing (cellChoices, generate, randomChoice)
import Html exposing (Html, button, div, input, option, p, select, text)
import Html.Attributes exposing (checked, disabled, name, selected, style, type_, value)
import Html.Events exposing (onClick, onInput)
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


type ShowWhat
    = ShowCellChoices
    | ShowBoard
    | ShowZeroes


textToShowWhat : String -> ShowWhat
textToShowWhat s =
    case s of
        "ShowBoard" ->
            ShowBoard

        "ShowZeroes" ->
            ShowZeroes

        _ ->
            ShowCellChoices


type alias Model =
    { kakuroModel : SharedTypes.Model
    , step : Maybe (Mdl -> Mdl)
    , showWhat : ShowWhat
    , kind : Int
    , boardNum : Int
    , row : Int
    , col : Int
    }


type Mdl
    = Mdl Model


initialKind : Int
initialKind =
    6


initialModel : Model
initialModel =
    { kakuroModel = initialKakuroModel initialKind
    , step = Nothing
    , showWhat = ShowCellChoices
    , kind = initialKind
    , boardNum = 0
    , row = 0
    , col = 0
    }


type Msg
    = Noop
    | Tick Posix
    | ChangeKind Int
    | StepCellChoices
    | GenerateChoices
    | SetShowWhat String


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

        ChangeKind newKind ->
            if model.kind == newKind then
                ( model, Cmd.none )

            else
                ( { model | kind = newKind }
                    |> clearGameState
                , Cmd.none
                )

        StepCellChoices ->
            ( { model
                | row = 0
                , col = 0
                , step = Just stepCellChoices
                , showWhat = ShowCellChoices
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
            ( doGameState (\gs -> { gs | hints = newHints })
                { newModel | showWhat = ShowCellChoices }
            , Cmd.none
            )

        SetShowWhat showWhatString ->
            let
                showWhat =
                    textToShowWhat showWhatString

                hints =
                    modelHints model
            in
            ( { model | showWhat = showWhat }
                |> clearGameState
                |> doHints (\_ -> hints)
                |> (case showWhat of
                        ShowCellChoices ->
                            identity

                        ShowBoard ->
                            guessRight

                        ShowZeroes ->
                            guessZeroes
                   )
            , Cmd.none
            )


guessZeroes : Model -> Model
guessZeroes model =
    let
        hints =
            Debug.log "guessZeros, hints" <|
                modelHints model

        setCell row col guesses =
            let
                val =
                    if List.member 0 <| Board.get row col hints then
                        9

                    else
                        1
            in
            Board.set row col val guesses
    in
    doGuesses (Generate.eachCell setCell)
        model


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
    let
        gameState =
            modelGameState model
    in
    if model.kind /= model.kakuroModel.kind then
        model
            |> doKakuroModel
                (\km -> { km | kind = model.kind })
            |> doGameState
                (\gs -> newBoardOfKind model.kind 1)

    else
        model
            |> doGameState
                (\gs ->
                    let
                        ( rows, cols ) =
                            ( gs.board.rows, gs.board.cols )
                    in
                    { gs
                        | guesses =
                            Board.make rows cols gs.board.defaultValue
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
            -- See the comment above the definition of `Generate.fixChoicesForSums`.
            -- It doesn't do anything
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

        {-
           ( ch, zeroOk ) =
               if List.member 0 choices then
                   ( LE.remove 0 choices, True )

               else
                   ( choices, False )
        -}
        ch =
            choices

        newModel =
            hintsSet row col ch model

        ( nextRow, nextCol, nextStep ) =
            if col >= cols - 1 then
                if row >= rows - 1 then
                    -- See the comment above the definition of
                    -- `Generate.fixChoicesForSumsInternal`.
                    -- It doesn't do anything
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


radio : String -> Bool -> msg -> Html msg
radio value isChecked msg =
    Html.span [ onClick msg ]
        [ input
            [ type_ "radio"
            , name "board-size"
            , checked isChecked
            ]
            []
        , text value
        ]


view : Model -> Html Msg
view model =
    div [ style "margin" "2em" ]
        [ h2 "TestGenerate"
        , p []
            [ p []
                [ radio "6" (model.kind == 6) (ChangeKind 6)
                , radio "8" (model.kind == 8) (ChangeKind 8)
                , radio "10" (model.kind == 10) (ChangeKind 10)
                ]
            , button
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
            , p []
                [ b "Show: "
                , select [ onInput SetShowWhat ]
                    [ option
                        [ value "ShowCellChoices"
                        , selected <| model.showWhat == ShowCellChoices
                        ]
                        [ text "Cell Choices" ]
                    , option
                        [ value "ShowBoard"
                        , selected <| model.showWhat == ShowBoard
                        ]
                        [ text "Board" ]
                    , option
                        [ value "ShowZeroes"
                        , selected <| model.showWhat == ShowZeroes
                        ]
                        [ text "Zeroes" ]
                    ]
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


realBoardIndex : IntBoard -> Int
realBoardIndex board =
    case board.index of
        Nothing ->
            0

        Just index ->
            index


newBoardOfKind : Int -> Int -> GameState
newBoardOfKind kind idx =
    let
        board =
            PuzzleDB.getBoardOfKind kind 1

        state =
            RenderBoard.makeGameState board

        flags =
            state.flags
    in
    { state
        | flags =
            { flags
                | showPossibilities = False
                , isHintInput = True
            }
    }


initialKakuroModel : Int -> SharedTypes.Model
initialKakuroModel kind =
    let
        state =
            newBoardOfKind kind 1

        idx =
            realBoardIndex state.board
    in
    { kind = kind
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


guessRight : Model -> Model
guessRight model =
    let
        board =
            modelBoard model

        setCell row col guesses =
            Board.set row col (Board.get row col board) guesses
    in
    doGuesses (Generate.eachCell setCell)
        model


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
