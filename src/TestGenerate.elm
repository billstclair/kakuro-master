module TestGenerate exposing (main)

{-| TODO:

Generate.cellChoices needs to prevent single-wide snakes at edges
(and elsewhere?) This can be requiring a 0 square at the edge or
not allowing a zero square one from the edge.

Play mode. Try out a generated game, to see if it is soluble.

-}

import Board exposing (Board)
import BoardSize
import Browser
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Entities exposing (nbsp)
import Generate exposing (GenerateRowState)
import Html exposing (Html, a, button, div, input, option, p, select, span, text)
import Html.Attributes exposing (checked, disabled, href, name, selected, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import List.Extra as LE
import PuzzleDB
import Random
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
import Task
import Time exposing (Posix)


type ShowWhat
    = ShowCellChoices
    | ShowBoard


textToShowWhat : String -> ShowWhat
textToShowWhat s =
    case s of
        "ShowBoard" ->
            ShowBoard

        _ ->
            ShowCellChoices


type alias Model =
    { error : Maybe String
    , kakuroModel : SharedTypes.Model
    , seed : Random.Seed
    , generateRowState : Maybe GenerateRowState
    , step : Maybe (Mdl -> Mdl)
    , showWhat : ShowWhat
    , kind : Int
    , boardNum : Int
    , row : Int
    , col : Int
    , clickedId : Maybe String
    }


type Mdl
    = Mdl Model


initialKind : Int
initialKind =
    6


initialModel : Model
initialModel =
    { error = Nothing
    , kakuroModel = initialKakuroModel initialKind
    , seed = Random.initialSeed 0
    , generateRowState = Nothing
    , step = Nothing
    , showWhat = ShowCellChoices
    , kind = initialKind
    , boardNum = 0
    , row = 0
    , col = 0
    , clickedId = Nothing
    }


type Msg
    = Noop
    | InitSeed Posix
    | Tick Posix
    | ChangeKind Int
    | Generate
    | GenerateStep
    | StepCellChoices
    | GenerateChoices
    | CancelSteps
    | SetShowWhat ShowWhat
    | ClickCell String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        mdl =
            case msg of
                Tick _ ->
                    model

                _ ->
                    { model
                        | error = Nothing
                        , clickedId = Nothing
                    }
    in
    updateInternal msg mdl


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        InitSeed posix ->
            ( { model | seed = Random.initialSeed <| Time.posixToMillis posix }
            , Cmd.none
            )

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

        Generate ->
            generate model |> withNoCmd

        GenerateStep ->
            generateStep model |> withNoCmd

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

        CancelSteps ->
            { model
                | step = Nothing
                , generateRowState = Nothing
            }
                |> withNoCmd

        SetShowWhat showWhat ->
            let
                --showWhat =
                --  textToShowWhat showWhatString
                hints =
                    modelHints model
            in
            { model | showWhat = showWhat }
                |> clearGameState
                |> doHints (\_ -> hints)
                |> (case showWhat of
                        ShowCellChoices ->
                            identity

                        ShowBoard ->
                            guessRight
                   )
                |> withNoCmd

        ClickCell id ->
            { model | clickedId = Just id }
                |> withNoCmd


generate : Model -> Model
generate model =
    let
        ( mdl, state ) =
            generateStepInternal model
    in
    if mdl.generateRowState == Nothing then
        finishGenerate mdl state

    else
        generate mdl


generateStepInternal : Model -> ( Model, GenerateRowState )
generateStepInternal model =
    let
        board =
            modelBoard model

        rows =
            board.rows

        cols =
            board.cols

        state =
            case model.generateRowState of
                Just grs ->
                    grs

                Nothing ->
                    Generate.initialGenerateRowState rows cols

        ( newState, nextSeed ) =
            Generate.generateRowStep state model.seed
    in
    ( { model
        | error =
            if newState.success then
                Nothing

            else
                Just "Generate error"
        , row = newState.row
        , col = newState.col
        , generateRowState =
            if newState.done then
                Nothing

            else
                Just newState
        , seed = nextSeed
      }
    , newState
    )


fillChoices : Model -> Model
fillChoices model =
    let
        board =
            modelBoard model

        rows =
            board.rows

        cols =
            board.cols

        get r c =
            Board.get r c board

        mr =
            model.row

        mc =
            model.col

        eachCol : Int -> Int -> Model -> Model
        eachCol row c m =
            if c >= cols then
                m

            else
                let
                    newChoices =
                        if (row == mr && c > mc) || (row > mr) then
                            [ 0 ]

                        else
                            Generate.cellChoices row c board
                in
                eachCol row (c + 1) <|
                    doHints
                        (Board.set row c newChoices)
                        m

        eachRow row m =
            if row >= rows then
                m

            else
                eachRow (row + 1) <|
                    eachCol row 0 m
    in
    eachRow 0 model


finishGenerate : Model -> GenerateRowState -> Model
finishGenerate model state =
    { model | showWhat = ShowBoard }
        |> doGameState
            (\gs -> newGameState state.board)
        |> guessRight
        |> fillChoices


generateStep : Model -> Model
generateStep model =
    let
        ( mdl, state ) =
            generateStepInternal model
    in
    if not state.success then
        { mdl | error = Just "Generate failed!" }

    else
        case mdl.generateRowState of
            Nothing ->
                finishGenerate mdl state

            Just s ->
                if Board.get s.row s.col s.board == 0 then
                    generateStep mdl

                else
                    finishGenerate mdl state


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


radio : String -> String -> Bool -> msg -> Html msg
radio radioName value isChecked msg =
    Html.span [ onClick msg ]
        [ input
            [ type_ "radio"
            , name radioName
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
            [ text "I'm working on generating new boards. This is a tool for doing that." ]
        , p []
            [ case model.error of
                Nothing ->
                    text nbsp

                Just err ->
                    span [ style "color" "red" ]
                        [ text err ]
            ]
        , p []
            [ p []
                [ radio "board-size" "6" (model.kind == 6) (ChangeKind 6)
                , radio "board-size" "8" (model.kind == 8) (ChangeKind 8)
                , radio "board-size" "10" (model.kind == 10) (ChangeKind 10)
                ]
            , let
                ( stepLabel, allLabel ) =
                    if model.generateRowState == Nothing then
                        ( "Start Generate", "Generate" )

                    else
                        ( "Generate Step", "Finish" )
              in
              p []
                [ button
                    [ onClick GenerateStep
                    , disabled <| model.step /= Nothing
                    ]
                    [ text stepLabel ]
                , text " "
                , button
                    [ onClick <| Generate
                    , disabled <| model.step /= Nothing
                    ]
                    [ text allLabel ]
                ]
            , p []
                [ button
                    [ onClick StepCellChoices
                    , disabled <|
                        (model.step /= Nothing)
                            || (model.generateRowState /= Nothing)
                    ]
                    [ text "StepCellChoices" ]
                , text " "
                , button
                    [ onClick GenerateChoices
                    , disabled <|
                        (model.step /= Nothing)
                            || (model.generateRowState /= Nothing)
                    ]
                    [ text "GenerateChoices" ]
                ]
            , p []
                [ button
                    [ onClick CancelSteps
                    , disabled <|
                        (model.step == Nothing)
                            && (model.generateRowState == Nothing)
                    ]
                    [ text "Cancel Steps" ]
                ]
            , p []
                [ b "Show: "
                , radio "show" "Cell choices" (model.showWhat == ShowCellChoices) <|
                    SetShowWhat ShowCellChoices
                , radio "show" "Board" (model.showWhat == ShowBoard) <|
                    SetShowWhat ShowBoard
                ]

            {--
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
                    ]
                    --}
            ]
        , p []
            [ b "row: "
            , text <| String.fromInt <| model.row
            , text " "
            , b "col: "
            , text <| String.fromInt <| model.col
            ]

        -- For some reason, lazy doesn't work here
        , p []
            [ Html.map msgMap <|
                Lazy.lazy
                    (RenderBoard.renderInternal <| model.showWhat == ShowBoard)
                    model.kakuroModel
            ]
        , case model.clickedId of
            Nothing ->
                text "Click on a cell for its hints."

            Just id ->
                case idToRowCol id of
                    Just ( row, col ) ->
                        let
                            hint =
                                Board.get row col <| modelHints model
                        in
                        p []
                            [ b "hints"
                            , text "["
                            , text <| String.fromInt row
                            , text ","
                            , text <| String.fromInt col
                            , text "] = "
                            , text <| Debug.toString hint
                            ]

                    Nothing ->
                        text ""
        , p []
            [ a [ href "./" ]
                [ text "Kakuro Dojo" ]
            ]
        ]


idToRowCol : String -> Maybe ( Int, Int )
idToRowCol id =
    case String.split "," id of
        [ _, rowString, colString ] ->
            case ( String.toInt rowString, String.toInt colString ) of
                ( Just row, Just col ) ->
                    Just ( row, col )

                _ ->
                    Nothing

        _ ->
            Nothing


msgMap : SharedTypes.Msg -> Msg
msgMap stm =
    case stm of
        SharedTypes.ClickCell id ->
            ClickCell id

        _ ->
            Noop


realBoardIndex : IntBoard -> Int
realBoardIndex board =
    case board.index of
        Nothing ->
            0

        Just index ->
            index


newBoardOfKind : Int -> Int -> GameState
newBoardOfKind kind idx =
    newGameState <| PuzzleDB.getBoardOfKind kind 1


newGameState : IntBoard -> GameState
newGameState board =
    let
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
        { init = \_ -> ( initialModel, Task.perform InitSeed Time.now )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 200 Tick
        ]
