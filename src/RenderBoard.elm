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

module RenderBoard exposing ( makeGameState
                            , render
                            , renderKeypad
                            )

import SharedTypes exposing (GameState, Model, modelVersion
                            , BoardSizes
                            , Flags
                            , IntBoard
                            , BClassBoard
                            , Labels, LabelsBoard
                            , Selection
                            , Hints, HintsBoard
                            , Msg ( ClickCell, PressKey
                                  , ToggleShowPossibilities
                                  , ToggleHintInput)
                            )
import Styles.Board exposing (class, classes, BClass(..))
import BoardSize
import Board exposing(Board, get, set)
import PuzzleDB
import Entities exposing (nbsp, copyright)
import Events exposing (onClickWithId, onClickWithInt, svgOnClickWithId)
import PlayHelpers exposing (isAllDone, computeFilledCellClasses, possibilities)
import Array exposing (Array)
import Char
import String
import List exposing (map)
import List.Extra as LE

import Debug exposing (log)

import Json.Decode as Json

import Html exposing
  (Html, Attribute, div, text, table, tr, td, th, a, img, button)
import Html.Attributes
  exposing (style, value, href, src, title, alt, id, autofocus)
import Html.Events exposing (on, onClick)
import Svg exposing (svg, line, rect, g)
import Svg.Attributes exposing ( x, y, width, height, x1, y1, x2, y2
                               , fill, stroke, fontSize )

-- I wanted to make GameState be an extensible record type,
-- but I couldn't figure it out, so I have to copy stuff. Yuck.
type alias RenderState =
  { board : IntBoard
  , labels : LabelsBoard
  , allDone : Bool
  , guesses : IntBoard
  , hints : HintsBoard
  , flags : Flags
  , selection : Maybe Selection
  -- Added to GameState            
  , cellClasses : BClassBoard }

makeRenderState : GameState -> BClassBoard -> Bool -> RenderState
makeRenderState state cellClasses allDone =
  { board = state.board
  , labels = state.labels
  , guesses = state.guesses
  , hints = state.hints
  , flags = state.flags
  , selection = state.selection
  -- Added to state
  , cellClasses = cellClasses
  , allDone = allDone
  }

br : Html a
br =
  Html.br [][]

cellId : Int -> Int -> Attribute m
cellId row col =
  id ((toString row) ++ "," ++ (toString col))

emptyLabels : Labels
emptyLabels = (0, 0)

emptyHints : Hints
emptyHints = []

defaultFlags : Flags
defaultFlags =
  { isHintInput = False
  , showPossibilities = True
  }

sumColLoop : Int -> Int -> Int -> (IntBoard) -> Int
sumColLoop row col sum board =
  let elt = get row (col-1) board
  in
      if elt == 0 then
        sum
      else
        sumColLoop (row+1) col (sum + elt) board

sumCol : Int -> Int -> IntBoard -> Int
sumCol row col board =
  sumColLoop row col 0 board

sumRowLoop : Int -> Int -> Int -> (IntBoard) -> Int
sumRowLoop row col sum board =
  let elt = get (row-1) col board
  in
      if elt == 0 then
        sum
      else
        sumRowLoop row (col+1) (sum + elt) board    

sumRow : Int -> Int -> IntBoard -> Int
sumRow row col board =
  sumRowLoop row col 0 board

computeLabel : Int -> Int -> LabelsBoard -> IntBoard -> LabelsBoard
computeLabel row col res board =
  if (get (row-1) (col-1) board) /= 0 then
    res
  else
    let rowsum = sumRow row col board
        colsum = sumCol row col board
    in
        if rowsum==0 && colsum == 0 then
          res
        else
          set row col (rowsum, colsum) res

computeLabelsColsLoop : Int -> Int -> LabelsBoard -> IntBoard -> LabelsBoard
computeLabelsColsLoop row col res board =
  if col >= res.cols then
    res
  else
    computeLabelsColsLoop row
                          (col+1)
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
    computeLabelsRowLoop (row+1)
                         (computeLabelsCols row res board)
                         board

computeLabelsRow : Int -> IntBoard -> LabelsBoard
computeLabelsRow row board =
  let res = Board.make (board.rows+1) (board.cols+1) emptyLabels
  in
      computeLabelsRowLoop row res board

computeLabels : IntBoard -> LabelsBoard
computeLabels board =
  computeLabelsRow 0 board

makeGameState : IntBoard -> GameState
makeGameState board =
  let rows = board.rows
      cols = board.cols
      guesses = Board.make rows cols board.defaultValue
      hints = Board.make rows cols emptyHints
  in
      { version = modelVersion
      , board = board
      , labels = (computeLabels board)
      , allDone = False
      , guesses = guesses
      , hints = hints
      , flags = defaultFlags
      , selection = Nothing
      }

toTwoDigitString : Int -> String
toTwoDigitString x =
  let str = toString x
  in
      if String.length str == 1 then
        nbsp ++ str
      else
        str

svgLabelTextHtml : Int -> BoardSize.Rect -> BoardSizes -> ( BoardSize.Rect -> (Int, Int) ) -> List (Html msg)
svgLabelTextHtml label cr sizes labelLocation =
  let (blx, bly) = labelLocation cr
  in
      [ Svg.text' [ svgClass "SvgLabelText"
                  , fontSize (toString sizes.labelFontSize)
                  , x (toString blx), y (toString bly)
                  ]
          [ Svg.text (toTwoDigitString label) ]
      ]            
            
svgLabelHtml : (Int, Int) -> BoardSizes -> BoardSize.Rect -> BoardSize.Rect -> List (Html msg)
svgLabelHtml label sizes cr bgr =
  let res =
        [ rect [ svgClass "SvgLabel"
               , x (toString bgr.x), y (toString bgr.y)
               , width (toString bgr.w), height (toString bgr.h)
               ]
            []
        , line [ svgClass "SvgSlash"
               , x1 (toString cr.x)
               , y1 (toString cr.y)
               , x2 (toString (cr.x + cr.w))
               , y2 (toString (cr.y + cr.h))
               ]
            []
        ]
      (right, bottom) = label
      res2 = if bottom == 0 then
               res
             else
               List.append res
                 <| svgLabelTextHtml bottom cr sizes BoardSize.bottomLabelLocation

  in
      if right == 0 then
        res2
      else
        List.append res2
          <| svgLabelTextHtml right cr sizes BoardSize.rightLabelLocation
          
svgHintTexts : List Int -> BoardSizes -> BoardSize.Rect -> List (Html msg) -> List (Html msg)
svgHintTexts hints sizes cr res =
  case hints of
      [] ->
        List.reverse res
      ( hint :: tail ) ->
        let ( blx, bly ) = BoardSize.hintTextLocation hint cr
            html = Svg.text' [ svgClass "SvgHintText"
                             , fontSize (toString sizes.hintFontSize)
                             , x (toString blx)
                             , y (toString bly)
                             ]
                     [ Svg.text (toString hint) ]
        in
            svgHintTexts tail sizes cr (html :: res)

renderSvgCell : Int -> Int -> BoardSizes -> RenderState -> Html Msg
renderSvgCell row col sizes state =
  let cr = BoardSize.cellRect row col sizes
      value = Board.get (row-1) (col-1) state.board
      (brow, bcol) = (row-1, col-1)
      label = if value == 0 then Board.get row col state.labels else (0, 0)
      guess = if value /= 0 then Board.get brow bcol state.guesses else 0
      hints = if value /= 0 && guess == 0 then
                Board.get brow bcol state.hints
              else
                []
      allDone = state.allDone
      errorClass = if value == 0 then
                    SvgCell
                  else
                    case Board.get brow bcol state.cellClasses of
                        Nothing -> SvgCell
                        Just c -> c
      selection = state.selection
      isSelected = case selection of
                       Nothing -> False
                       Just sel -> sel == (row-1, col-1)
      colorClass = if value == 0 then
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
      cellClass = if value == 0 then
                    if label == emptyLabels then
                      "SvgEmptyCell"
                    else
                      "SvgCell SvgCellColor"
                  else
                    (if isSelected then "SvgSelected " else "")
                    ++ "SvgCell "
                    ++ colorClass
      cr2 = if isSelected then BoardSize.insetRectForSelection cr else cr
      rectHtml = rect [ svgClass cellClass
                      , x (toString cr2.x), y (toString cr2.y)
                      , width (toString cr2.w), height (toString cr2.h)
                      ]
                      []
  in
      g []
        (if value /= 0 then
           let clickRect = rect [ svgClass "SvgClick"
                                , x (toString cr.x), y (toString cr.y)
                                , width (toString cr.w), height (toString cr.h)
                                , cellId brow bcol
                                , svgOnClickWithId ClickCell
                                ]
                                []
           in
               if guess /= 0 then
                 let (tx, ty) = BoardSize.cellTextLocation cr
                 in
                     [ rectHtml
                     , Svg.text' [ svgClass "SvgCellText"
                                 , fontSize (toString sizes.cellFontSize)
                                 , x (toString tx), y (toString ty)
                                 ]
                       [ Svg.text (toString guess) ]
                     , clickRect
                     ]
               else
                 ( rectHtml ::
                     (List.append
                        (svgHintTexts hints sizes cr [])
                        [ clickRect ]
                     )
                 )
         else if label == (0, 0) then
           [ rectHtml ]
         else
           let bgr = BoardSize.labelBackgroundRect cr
           in
               ( List.append
                   (svgLabelHtml label sizes cr bgr)
                   [ rectHtml ]
               )
        )

renderSvgCells : Int -> Int -> Int -> List (Html Msg) -> BoardSizes -> RenderState -> List (Html Msg)
renderSvgCells row col cols res sizes state =
  if col >= cols then
    List.reverse res
  else
    let cellHtml = renderSvgCell row col sizes state
    in
        renderSvgCells row (col+1) cols (cellHtml :: res) sizes state

renderSvgRow : Int -> BoardSizes -> RenderState -> Html Msg
renderSvgRow row sizes state =
  g []
    <| renderSvgCells row 0 state.labels.cols [] sizes state

renderSvgRows : Int -> Int -> List (Html Msg) -> BoardSizes -> RenderState -> List (Html Msg)
renderSvgRows row rows res sizes state =
  if row >= rows then
    List.reverse res
  else
    let rowHtml = renderSvgRow row sizes state
    in
        renderSvgRows (row+1) rows (rowHtml :: res) sizes state

getBoardSizes : Model -> BoardSizes
getBoardSizes model =
  case model.boardSizes of
      Nothing -> BoardSize.computeBoardSizes model
      Just bs -> bs  

renderSvgBoard : Model -> Html Msg
renderSvgBoard model =
  let sizes = getBoardSizes model
      state = model.gameState
      size = toString sizes.boardSize
      cellClasses = computeFilledCellClasses state.board state.guesses
      allDone = isAllDone state.board state.guesses
      state2 = makeRenderState state cellClasses allDone
  in
      svg [ width size, height size ]
        ((rect [ svgClass "SvgCell SvgCellColor", width size, height size ] [])
        ::
           (renderSvgRows 0 state2.labels.rows [] sizes state2))

svgClass : String -> Attribute msg
svgClass = Svg.Attributes.class

helperLoop : (Int, Int) -> Int -> (Int, Int) -> IntBoard -> IntBoard -> (Int, Int, List Int) -> (Int, Int, List Int)
helperLoop start cnt inc board guesses res =
  if cnt <= 0 then
    res
  else
    let (row, col) = start
        value = Board.get row col board
        guess = Board.get row col guesses
    in
        if  value == 0 then
          res
        else
          let (ri, ci) = inc
              (zeroes, sum, nums) = res
              zeroes' = if guess == 0 then zeroes+1 else zeroes
              sum' = sum + value
              nums' = if guess == 0 then nums else (guess :: nums)
          in
              helperLoop (row+ri, col+ci) (cnt-1) inc board guesses (zeroes', sum', nums')

maxHelperLen : Int
maxHelperLen = 200              -- needs to be computed on window width

helperText : (Int, Int) -> (Int, Int) -> ((Int, Int) -> Int) -> GameState -> String
helperText inc neginc acc state =
  let board = state.board
      guesses = state.guesses
  in
      case state.selection of
          Nothing -> ""
          Just loc ->
            let (row, col) = loc
                (ri, ci) = neginc
                rc = acc loc
                (zeroes, sum, nums) =
                  helperLoop loc (10 - rc) inc
                    board guesses (0, 0, [])
                (zeroes', sum', nums') =
                  helperLoop (row+ri, col+ci) rc neginc
                    board guesses (zeroes, sum, nums)
                leftsum = sum' - (List.foldr (+) 0 nums')
                run = possibilities leftsum zeroes' nums'
                runlen = List.length run
                maxRunlen = maxHelperLen // (zeroes'+1)
                run' = List.take maxRunlen run
            in
                String.append
                  (List.map (\x -> List.map toString x) run'
                  |> List.map String.concat
                  |> String.join " "
                  )
                  <| if runlen > maxRunlen then "..." else ""

rowHelperText : Model -> String
rowHelperText model =
  helperText (0, 1) (0, -1) snd model.gameState

colHelperText : Model -> String
colHelperText model =
  helperText (1, 0) (-1, 0) fst model.gameState

render : Model -> Html Msg
render model =
  div []
    [ Styles.Board.style
    , renderSvgBoard model
    , if model.gameState.flags.showPossibilities then
        div [ class Helper ]
          [ text <| "row: " ++ (rowHelperText model)
          , br
          , text <| "col: " ++ (colHelperText model)
          , br
          ]
      else
        br
    ]

--
-- The push-button keypad
--

keypadButtonClass : String -> GameState -> Attribute Msg
keypadButtonClass label state =
  let highlight =
        if label == "*" then
          state.flags.showPossibilities
        else if label == "#" then
          state.flags.isHintInput
        else
          False
      highlightClasses = if highlight then
                           [KeypadButtonHighlight]
                         else
                           []
  in
      classes (KeypadButton :: highlightClasses)

keycodeCell : Int -> String -> String -> GameState -> Html Msg
keycodeCell keycode label fontSize state =
  td [ class KeypadTd
     , if label == "*" then
         onClick ToggleShowPossibilities
       else if label == "#" then
         onClick ToggleHintInput
       else
         onClickWithInt PressKey keycode
     ]
    [ button [ keypadButtonClass label state
             , style [ ( "font-size", fontSize )
                     , ( "line-height", fontSize ) ]
             , autofocus (label == " ")]
        [ text  label ]
    ]

keypadAlist : List (Char, Int)
keypadAlist =
  [ ('^', Char.toCode 'i')
  , ('v', Char.toCode 'k')
  , ('<', Char.toCode 'j')
  , ('>', Char.toCode 'l')
  , ('*', Char.toCode '*')
  , ('#', Char.toCode '#')
  , (' ', Char.toCode '0')
  ]

keypadKeycode : Char -> Int
keypadKeycode char =
  if char >= '0' && char <= '9' then
    Char.toCode char
  else
    let pair = LE.find (\x -> (fst x) == char) keypadAlist
    in
        case pair of
          Nothing ->
            0
          Just (_, res) ->
            res

renderKeypadCell : Char -> String -> GameState -> Html Msg
renderKeypadCell char fontSize state =
  keycodeCell (keypadKeycode char) (String.fromList [char]) fontSize state

renderKeypadRow : String -> String -> GameState -> Html Msg
renderKeypadRow string fontSize state =
  let chars = String.toList string
  in
      tr []
        <| List.map (\x -> renderKeypadCell x fontSize state) chars

-- 1 2 3 ^
-- 4 5 6 v
-- 7 8 9 <
-- * 0 # >
renderKeypad : Model -> Html Msg
renderKeypad model =
  let boardSizes = getBoardSizes model
      keypadSize = (toString boardSizes.keypadSize) ++ "px"
      fontSize = (toString boardSizes.keypadFontSize) ++ "px"
      state = model.gameState
  in
      table [ class Table
            , style [ ("width", keypadSize)
                    , ("height", keypadSize)
                    ]
            ]
      [ Styles.Board.style
      , renderKeypadRow "123*" fontSize state
      , renderKeypadRow "456#" fontSize state
      , renderKeypadRow "78^ " fontSize state
      , renderKeypadRow "9<v>" fontSize state
      ]
