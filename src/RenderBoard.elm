----------------------------------------------------------------------
--
-- RenderBoard.elm
-- Render the game board.
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module RenderBoard exposing
    ( computeLabels
    , makeGameState
    , render
    , renderHelp
    , renderInternal
    , renderKeypad
    )

import Array exposing (Array)
import Board exposing (Board, get, set)
import BoardSize
import Char
import Debug exposing (log)
import Entities exposing (copyright, nbsp)
import Events exposing (onClickWithId, onClickWithString, svgOnClickWithId)
import Html exposing (Attribute, Html, a, button, div, img, table, td, text, th, tr)
import Html.Attributes exposing (alt, autofocus, href, id, src, style, title, value)
import Html.Events exposing (on, onClick)
import Json.Decode as Json
import List exposing (map)
import List.Extra as LE
import PlayHelpers exposing (computeFilledCellClasses, isAllDone, possibilities)
import PuzzleDB
import SharedTypes
    exposing
        ( BClassBoard
        , BoardSizes
        , ExploreState
        , Flags
        , GameState
        , Hints
        , HintsBoard
        , IntBoard
        , Labels
        , LabelsBoard
        , Model
        , Msg(..)
        , SavedModel
        , Selection
        , WindowSize
        )
import String
import Styles.Board exposing (BClass(..), class, classes)
import Svg exposing (Svg, g, line, rect, svg)
import Svg.Attributes
    exposing
        ( fill
        , fontSize
        , height
        , stroke
        , transform
        , width
        , x
        , x1
        , x2
        , y
        , y1
        , y2
        )



-- I wanted to make GameState be an extensible record type,
-- but I couldn't figure it out, so I have to copy stuff. Yuck.


type alias RenderState =
    { name : String
    , board : IntBoard
    , labels : LabelsBoard
    , guesses : IntBoard
    , hints : HintsBoard
    , flags : Flags
    , selection : Maybe Selection
    , exploreState : Maybe ExploreState
    , debugMode : Bool

    -- Added to GameState
    , allDone : Bool
    , cellClasses : BClassBoard
    }


makeRenderState : Bool -> String -> GameState -> BClassBoard -> Bool -> RenderState
makeRenderState debugMode name state cellClasses allDone =
    let
        flags =
            state.flags
    in
    { name = name
    , board = state.board
    , labels = state.labels
    , guesses = state.guesses
    , hints = state.hints
    , flags = state.flags
    , selection = state.selection
    , exploreState = state.exploreState
    , debugMode = debugMode

    -- Added to state
    , allDone = allDone
    , cellClasses = cellClasses
    }


br : Html a
br =
    Html.br [] []


cellId : String -> Int -> Int -> Attribute m
cellId name row col =
    id (name ++ "," ++ String.fromInt row ++ "," ++ String.fromInt col)


emptyLabels : Labels
emptyLabels =
    ( 0, 0 )


emptyHints : Hints
emptyHints =
    []


defaultFlags : Flags
defaultFlags =
    { isHintInput = False
    , showPossibilities = True
    , firstGuess = 0
    , keyClickSound = True
    }


sumColLoop : Int -> Int -> Int -> IntBoard -> Int
sumColLoop row col sum board =
    let
        elt =
            get row (col - 1) board
    in
    if elt == 0 then
        sum

    else
        sumColLoop (row + 1) col (sum + elt) board


sumCol : Int -> Int -> IntBoard -> Int
sumCol row col board =
    sumColLoop row col 0 board


sumRowLoop : Int -> Int -> Int -> IntBoard -> Int
sumRowLoop row col sum board =
    let
        elt =
            get (row - 1) col board
    in
    if elt == 0 then
        sum

    else
        sumRowLoop row (col + 1) (sum + elt) board


sumRow : Int -> Int -> IntBoard -> Int
sumRow row col board =
    sumRowLoop row col 0 board


computeLabel : Int -> Int -> LabelsBoard -> IntBoard -> LabelsBoard
computeLabel row col res board =
    if get (row - 1) (col - 1) board /= 0 then
        res

    else
        let
            rowsum =
                sumRow row col board

            colsum =
                sumCol row col board
        in
        if rowsum == 0 && colsum == 0 then
            res

        else
            set row col ( rowsum, colsum ) res


computeLabelsColsLoop : Int -> Int -> LabelsBoard -> IntBoard -> LabelsBoard
computeLabelsColsLoop row col res board =
    if col >= res.cols then
        res

    else
        computeLabelsColsLoop row
            (col + 1)
            (computeLabel row col res board)
            board


computeLabelsCols : Int -> LabelsBoard -> IntBoard -> LabelsBoard
computeLabelsCols row res board =
    computeLabelsColsLoop row 0 res board


computeLabelsRowLoop : Int -> LabelsBoard -> IntBoard -> LabelsBoard
computeLabelsRowLoop row res board =
    if row >= res.rows then
        res

    else
        computeLabelsRowLoop (row + 1)
            (computeLabelsCols row res board)
            board


computeLabelsRow : Int -> IntBoard -> LabelsBoard
computeLabelsRow row board =
    let
        res =
            Board.make (board.rows + 1) (board.cols + 1) emptyLabels
    in
    computeLabelsRowLoop row res board


computeLabels : IntBoard -> LabelsBoard
computeLabels board =
    computeLabelsRow 0 board


makeGameState : IntBoard -> GameState
makeGameState board =
    let
        rows =
            board.rows

        cols =
            board.cols

        guesses =
            Board.make rows cols board.defaultValue

        hints =
            Board.make rows cols emptyHints
    in
    { board = board
    , labels = computeLabels board
    , guesses = guesses
    , hints = hints
    , flags = defaultFlags
    , selection = Nothing
    , exploreState = Nothing
    }


toTwoDigitString : Int -> String
toTwoDigitString x =
    let
        str =
            String.fromInt x
    in
    if String.length str == 1 then
        nbsp ++ str

    else
        str



-- I really want to use the CSS classes from Styles/Board.elm
-- with class and id, but I get a JavaScript runtime error
-- when I do that:
-- Uncaught TypeError: Cannot assign to read only property 'className' of object '#<SVGTextElement>'(…)


svgClass : String -> Attribute msg
svgClass =
    Svg.Attributes.class


svgLabelTextHtml : Int -> BoardSize.Rect -> BoardSizes -> (BoardSize.Rect -> ( Int, Int )) -> List (Html msg)
svgLabelTextHtml label cr sizes labelLocation =
    let
        ( blx, bly ) =
            labelLocation cr
    in
    [ Svg.text_
        [ svgClass "SvgLabelText"
        , fontSize (String.fromInt sizes.labelFontSize)
        , x (String.fromInt blx)
        , y (String.fromInt bly)
        ]
        [ Svg.text (toTwoDigitString label) ]
    ]


svgLabelHtml : ( Int, Int ) -> BoardSizes -> BoardSize.Rect -> BoardSize.Rect -> List (Html msg)
svgLabelHtml label sizes cr bgr =
    let
        res =
            [ rect
                [ svgClass "SvgLabel"
                , x (String.fromInt bgr.x)
                , y (String.fromInt bgr.y)
                , width (String.fromInt bgr.w)
                , height (String.fromInt bgr.h)
                ]
                []
            , line
                [ svgClass "SvgSlash"
                , x1 (String.fromInt (cr.x + 1))
                , y1 (String.fromInt (cr.y + 1))
                , x2 (String.fromInt (cr.x + cr.w - 1))
                , y2 (String.fromInt (cr.y + cr.h - 1))
                ]
                []
            ]

        ( right, bottom ) =
            label

        res2 =
            if bottom == 0 then
                res

            else
                List.append res <|
                    svgLabelTextHtml bottom cr sizes BoardSize.bottomLabelLocation
    in
    if right == 0 then
        res2

    else
        List.append res2 <|
            svgLabelTextHtml right cr sizes BoardSize.rightLabelLocation


svgHintTexts : List Int -> BoardSizes -> BoardSize.Rect -> List (Html msg) -> List (Html msg)
svgHintTexts hints sizes cr res =
    case hints of
        [] ->
            List.reverse res

        hint :: tail ->
            let
                ( blx, bly ) =
                    BoardSize.hintTextLocation hint cr

                html =
                    if hint == 0 then
                        -- Happens in TextGenerate.elm
                        Svg.text_ [] []

                    else
                        Svg.text_
                            [ svgClass "SvgHintText"
                            , fontSize (String.fromInt sizes.hintFontSize)
                            , x (String.fromInt blx)
                            , y (String.fromInt bly)
                            ]
                            [ Svg.text <| String.fromInt hint ]
            in
            svgHintTexts tail sizes cr (html :: res)


renderSvgCell : Int -> Int -> BoardSizes -> RenderState -> Html Msg
renderSvgCell row col sizes state =
    let
        cr =
            BoardSize.cellRect row col sizes

        value =
            Board.get (row - 1) (col - 1) state.board

        ( brow, bcol ) =
            ( row - 1, col - 1 )

        label =
            if value == 0 then
                Board.get row col state.labels

            else
                ( 0, 0 )

        guess =
            if value /= 0 then
                Board.get brow bcol state.guesses

            else
                0

        isExploratory =
            if guess == 0 then
                False

            else
                case state.exploreState of
                    Nothing ->
                        False

                    Just es ->
                        guess == Board.get brow bcol es.guesses

        debugMode =
            state.debugMode

        hints =
            if debugMode || (value /= 0 && guess == 0) then
                Board.get brow bcol state.hints

            else
                []

        allDone =
            state.allDone

        errorClass =
            if value == 0 then
                SvgCell

            else
                case Board.get brow bcol state.cellClasses of
                    Nothing ->
                        SvgCell

                    Just c ->
                        c

        selection =
            state.selection

        isSelected =
            case selection of
                Nothing ->
                    False

                Just sel ->
                    sel == ( row - 1, col - 1 )

        colorClass =
            if value == 0 then
                "SvgCellColor"

            else if allDone then
                "SvgDoneColor"

            else if errorClass == Error then
                if isSelected then
                    "SvgSelectedErrorColor"

                else
                    "SvgErrorColor"

            else
                "SvgCellColor"

        cellClass =
            if value == 0 then
                if label == emptyLabels && not debugMode then
                    "SvgEmptyCell"

                else
                    "SvgCell SvgCellColor"

            else
                (if isSelected then
                    if sizes.cellSize < 50 then
                        "SvgSelectedSmall "

                    else
                        "SvgSelected "

                 else
                    ""
                )
                    ++ "SvgCell "
                    ++ colorClass

        cr2 =
            if isSelected then
                BoardSize.insetRectForSelection cr

            else
                cr

        rectHtml zeroHintp =
            rect
                [ svgClass
                    (cellClass
                        ++ (if zeroHintp && not debugMode then
                                " SvgDoneColor"

                            else if
                                (value == 0)
                                    && not zeroHintp
                                    && debugMode
                                    && (row /= 0)
                                    && (col /= 0)
                            then
                                " SvgErrorColor"

                            else
                                ""
                           )
                    )
                , x (String.fromInt cr2.x)
                , y (String.fromInt cr2.y)
                , width (String.fromInt cr2.w)
                , height (String.fromInt cr2.h)
                ]
                []
    in
    g []
        (if value /= 0 || (debugMode && row /= 0 && col /= 0) then
            let
                clickRect =
                    rect
                        [ svgClass "SvgClick"
                        , x (String.fromInt cr.x)
                        , y (String.fromInt cr.y)
                        , width (String.fromInt cr.w)
                        , height (String.fromInt cr.h)
                        , cellId state.name brow bcol
                        , svgOnClickWithId ClickCell
                        ]
                        []
            in
            if guess /= 0 then
                let
                    ( tx, ty ) =
                        BoardSize.cellTextLocation cr
                in
                [ rectHtml False
                , Svg.text_
                    [ svgClass
                        (if isExploratory then
                            "SvgCellText SvgKeypadExploratoryColor"

                         else
                            "SvgCellText"
                        )
                    , fontSize (String.fromInt sizes.cellFontSize)
                    , x (String.fromInt tx)
                    , y (String.fromInt ty)
                    ]
                    [ Svg.text (String.fromInt guess) ]
                , clickRect
                ]

            else
                rectHtml (List.member 0 hints)
                    :: List.append
                        (svgHintTexts hints sizes cr [])
                        [ clickRect ]

         else if label == ( 0, 0 ) then
            [ rectHtml False ]

         else
            let
                bgr =
                    BoardSize.labelBackgroundRect cr
            in
            List.append
                [ rectHtml False ]
                (svgLabelHtml label sizes cr bgr)
        )


renderSvgCells : Int -> Int -> Int -> List (Html Msg) -> BoardSizes -> RenderState -> List (Html Msg)
renderSvgCells row col cols res sizes state =
    if col >= cols then
        List.reverse res

    else
        let
            cellHtml =
                renderSvgCell row col sizes state
        in
        renderSvgCells row (col + 1) cols (cellHtml :: res) sizes state


renderSvgRow : Int -> BoardSizes -> RenderState -> Html Msg
renderSvgRow row sizes state =
    g [] <|
        renderSvgCells row 0 state.labels.cols [] sizes state


renderSvgRows : Int -> Int -> List (Html Msg) -> BoardSizes -> RenderState -> List (Html Msg)
renderSvgRows row rows res sizes state =
    if row >= rows then
        List.reverse res

    else
        let
            rowHtml =
                renderSvgRow row sizes state
        in
        renderSvgRows (row + 1) rows (rowHtml :: res) sizes state


getBoardSizes : Model -> BoardSizes
getBoardSizes model =
    case model.boardSizes of
        Nothing ->
            BoardSize.computeBoardSizes model

        Just bs ->
            bs


renderSvgBoard : Bool -> String -> Model -> Html Msg
renderSvgBoard debugMode name model =
    let
        sizes =
            getBoardSizes model

        state =
            model.gameState

        size =
            String.fromInt sizes.boardSize

        cellClasses =
            computeFilledCellClasses state.board state.guesses

        allDone =
            isAllDone state.board state.guesses

        state2 =
            makeRenderState debugMode name state cellClasses allDone
    in
    svg [ width size, height size ]
        (rect [ svgClass "SvgCell SvgCellColor", width size, height size ] []
            :: renderSvgRows 0 state2.labels.rows [] sizes state2
        )


helperLoop : ( Int, Int ) -> Int -> ( Int, Int ) -> IntBoard -> IntBoard -> ( Int, Int, List Int ) -> ( Int, Int, List Int )
helperLoop start cnt inc board guesses res =
    if cnt <= 0 then
        res

    else
        let
            ( row, col ) =
                start

            value =
                Board.get row col board

            guess =
                Board.get row col guesses
        in
        if value == 0 then
            res

        else
            let
                ( ri, ci ) =
                    inc

                ( zeroes, sum, nums ) =
                    res

                zeroes_ =
                    if guess == 0 then
                        zeroes + 1

                    else
                        zeroes

                sum_ =
                    sum + value

                nums_ =
                    if guess == 0 then
                        nums

                    else
                        guess :: nums
            in
            helperLoop ( row + ri, col + ci ) (cnt - 1) inc board guesses ( zeroes_, sum_, nums_ )


maxHelperLen : Int
maxHelperLen =
    200



-- needs to be computed on window width


helperText : ( Int, Int ) -> ( Int, Int ) -> (( Int, Int ) -> Int) -> GameState -> String
helperText inc neginc acc state =
    let
        board =
            state.board

        guesses =
            state.guesses
    in
    case state.selection of
        Nothing ->
            ""

        Just loc ->
            let
                ( row, col ) =
                    loc

                ( ri, ci ) =
                    neginc

                rc =
                    acc loc

                ( zeroes, sum, nums ) =
                    helperLoop loc
                        (10 - rc)
                        inc
                        board
                        guesses
                        ( 0, 0, [] )

                ( zeroes_, sum_, nums_ ) =
                    helperLoop ( row + ri, col + ci )
                        rc
                        neginc
                        board
                        guesses
                        ( zeroes, sum, nums )

                leftsum =
                    sum_ - List.foldr (+) 0 nums_

                run =
                    possibilities leftsum zeroes_ nums_

                runlen =
                    List.length run

                maxRunlen =
                    maxHelperLen // (zeroes_ + 1)

                run_ =
                    List.take maxRunlen run
            in
            String.append
                (List.map (\x -> List.map String.fromInt x) run_
                    |> List.map String.concat
                    |> String.join " "
                )
            <|
                if runlen > maxRunlen then
                    "..."

                else
                    ""


rowHelperText : Model -> String
rowHelperText model =
    helperText ( 0, 1 ) ( 0, -1 ) Tuple.second model.gameState


colHelperText : Model -> String
colHelperText model =
    helperText ( 1, 0 ) ( -1, 0 ) Tuple.first model.gameState


renderPossibilities : Model -> Html Msg
renderPossibilities model =
    if model.gameState.flags.showPossibilities then
        div [ class Helper ]
            [ div [ class HelperLine ]
                [ text <| "row: " ++ rowHelperText model ]
            , div [ class HelperLine ]
                [ text <| "col: " ++ colHelperText model ]
            ]

    else
        br


render : Model -> Html Msg
render model =
    renderInternal False model


renderInternal : Bool -> Model -> Html Msg
renderInternal debugMode model =
    div []
        [ Styles.Board.style
        , renderSvgBoard debugMode "" model
        , renderPossibilities model
        ]


renderHelp : String -> Model -> WindowSize -> Html Msg
renderHelp name model windowSize =
    let
        m =
            { model | windowSize = Just windowSize }
    in
    div []
        [ Styles.Board.style
        , renderSvgBoard False name m
        , case model.gameState.selection of
            Nothing ->
                br

            Just _ ->
                renderPossibilities m
        ]



--
-- The push-button keypad
--


keypadTextClass : String -> GameState -> String
keypadTextClass label state =
    let
        isDigit =
            String.contains label "123456789"

        highlight =
            if isDigit || (label == "#") then
                state.flags.isHintInput

            else
                False

        exploratory =
            if highlight then
                False

            else if isDigit then
                state.exploreState /= Nothing

            else
                False

        firstGuess =
            state.flags.firstGuess

        digit =
            if not isDigit || (firstGuess == 0) then
                0

            else
                case String.toInt label of
                    Just d ->
                        d

                    _ ->
                        0

        color =
            if highlight then
                "SvgKeypadHighlightColor"

            else if exploratory || (firstGuess /= 0 && firstGuess == digit) then
                "SvgKeypadExploratoryColor"

            else
                "SvgKeypadColor"
    in
    "SvgKeypadText " ++ color


keycodeCell : String -> String -> String -> Int -> String -> GameState -> Html Msg
keycodeCell key cx cy cellSize fontsize state =
    let
        msg =
            if key == "*" then
                onClick OpenStarMenu

            else if key == "#" then
                onClick ToggleHintInput

            else
                onClickWithString (DownKey True) <| keypadKeycode key

        cs =
            String.fromInt cellSize

        fx =
            (7 * cellSize) // 32

        fy =
            13 * cellSize // 16
    in
    g [ transform <| "translate(" ++ cx ++ "," ++ cy ++ ")" ]
        [ rect
            [ svgClass "SvgKeypad"
            , width cs
            , height cs
            ]
            []
        , Svg.text_
            [ svgClass <| keypadTextClass key state
            , x <| String.fromInt fx
            , y <| String.fromInt fy
            , fontSize fontsize
            ]
            [ Svg.text key ]
        , rect
            [ svgClass "SvgClick"
            , width cs
            , height cs
            , msg
            ]
            []
        ]


keypadAlist : List ( String, String )
keypadAlist =
    [ ( "^", "i" )
    , ( "v", "k" )
    , ( "<", "j" )
    , ( ">", "l" )
    , ( "*", "*" )
    , ( "#", "#" )
    , ( " ", "0" )
    ]


keypadKeycode : String -> String
keypadKeycode char =
    if char >= "0" && char <= "9" then
        char

    else
        let
            pair =
                LE.find (\x -> Tuple.first x == char) keypadAlist
        in
        case pair of
            Nothing ->
                char

            Just ( _, res ) ->
                res


renderKeypadCell : String -> String -> Int -> Int -> String -> GameState -> Svg Msg
renderKeypadCell char cy col cellSize fontSize state =
    let
        cx =
            String.fromInt (1 + col * (cellSize + 1))

        cs =
            String.fromInt cellSize
    in
    keycodeCell
        char
        cx
        cy
        cellSize
        fontSize
        state


renderKeypadRow : Int -> String -> Int -> String -> GameState -> Svg Msg
renderKeypadRow row string cellSize fontSize state =
    let
        y =
            1 + row * (cellSize + 1)

        cy =
            String.fromInt y

        chars =
            String.toList string |> List.map String.fromChar
    in
    g [] <|
        List.map2
            (\char col ->
                renderKeypadCell char cy col cellSize fontSize state
            )
            chars
            (List.range 0 <| List.length chars)



-- 1 2 3 ^
-- 4 5 6 v
-- 7 8 9 <
-- * 0 # >


renderKeypad : Model -> Html Msg
renderKeypad model =
    let
        boardSizes =
            getBoardSizes model

        cellSize =
            (boardSizes.keypadSize - 5) // 4

        keypadSize =
            String.fromInt (cellSize * 4 + 5)

        fontSize =
            String.fromInt boardSizes.keypadFontSize ++ "px"

        state =
            model.gameState
    in
    div []
        [ Styles.Board.style
        , svg [ width keypadSize, height keypadSize ]
            [ rect
                [ svgClass "SvgCell SvgCellColor"
                , x "0"
                , y "0"
                , width keypadSize
                , height keypadSize
                ]
                []
            , renderKeypadRow 0 "123*" cellSize fontSize state
            , renderKeypadRow 1 "456#" cellSize fontSize state
            , renderKeypadRow 2 "78^ " cellSize fontSize state
            , renderKeypadRow 3 "9<v>" cellSize fontSize state
            ]
        ]
