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

import SharedTypes exposing (GameState, modelVersion
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
import Board exposing(Board, get, set)
import PuzzleDB
import Entities exposing (nbsp, copyright)
import Events exposing (onClickWithId, onClickWithInt)
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

classedCell : Int -> Int -> Int -> List BClass -> Html Msg
classedCell num row col classTypes =
  td [ classes <| CellTd :: classTypes
     , cellId row col
     , onClickWithId ClickCell
     ]
    [ text <| toString num ]

cell : Bool -> Int -> Int -> Int -> Html Msg
cell isSelected num row col =
  classedCell num row col (if isSelected then [ Selected ] else [])

emptyCell : Html a
emptyCell =
  td [ classes [ CellTd, EmptyCellBackground ] ]
    [ div [ class EmptyCell ]
        [ text nbsp ]
    ]

unfilledCell : Bool -> Int -> Int -> Html Msg
unfilledCell isSelected row col =
  let cls = if isSelected then
              classes [CellTd, Selected]
            else
              class CellTd
  in
      td [ cls
         , onClickWithId ClickCell
         ]
      [ div [ class UnfilledCell
            , cellId row col
            ]
          [ text nbsp ]
    ]

errorCell : Int -> Int -> Int -> Html Msg
errorCell num row col =
  classedCell num row col [ Error ]

selectedCell : Int -> Int -> Int -> Html Msg
selectedCell num row col =
  classedCell num row col [ Selected ]

selectedErrorCell : Int -> Int -> Int -> Html Msg
selectedErrorCell num row col =
  classedCell num row col [ SelectedError ]

hintChars : List Int -> Int -> String
hintChars hints hint =
  String.append (if (List.member hint hints) then toString hint else nbsp)
                nbsp

hintRow : Int -> Int -> List Int -> Html a
hintRow min max hints =
  [min .. max]
  |> map (hintChars hints)
  |> String.concat
  |> String.left (2*(max-min)+1)
  |> text

hintCell : Bool -> List Int -> Int -> Int -> Html Msg
hintCell isSelected hints row col =
  td [ if isSelected then
              classes [ Hint, Selected ]
            else
              class Hint
     ]
    [ div [ cellId row col
          , onClickWithId ClickCell
          ]
        [ hintRow 1 3 hints
        , br
        , hintRow 4 6 hints
        , br
        , hintRow 7 9 hints
        ]
    ]

labelCell : Int -> Int -> Html a
labelCell right bottom =
  td []
    [ div [ class Label ]
        [ table [ class LabelTable ]
            [ tr [ class LabelTr ]
                [ td [ class LabelTd ] [ text nbsp ]
                , td [ class RightLabelTd ]
                  [ text <| if right == 0 then nbsp else (toString right) ]
                ]
            , tr [ class LabelTr ]
              [ td [ class BottomLabelTd ]
                  [ text <| if bottom == 0 then nbsp else (toString bottom) ]
              , td [ class LabelTd ] [ text nbsp ]
              ]
            ]
        ]
    ]

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

renderFilledCell : Bool -> Int -> Int -> Int -> RenderState -> Html Msg
renderFilledCell isSelected num row col state =
  let maybeClass = if state.allDone then
                     Just Done
                   else
                     Board.get row col state.cellClasses
      classes = case maybeClass of
                    Nothing -> []
                    Just a -> [ a ]
      selectedClasses = case maybeClass of
                            Just Error -> [ SelectedError ]
                            _ -> Selected :: classes
      classes2 = if isSelected then selectedClasses else classes
  in
      classedCell num row col classes2

renderCell : Int -> Int -> RenderState -> Html Msg
renderCell row col state =
  let labels = state.labels
      (right, bottom) = get row col labels
      bRow = row - 1
      bCol = col - 1
      selection = state.selection
      isSelected = case selection of
                     Nothing -> False
                     Just x -> x == (bRow, bCol)
  in
      if right==0 && bottom==0 then
        let val = (get bRow bCol state.guesses)
            hints = (get bRow bCol state.hints)
        in
            if val == 0 then
              if (get bRow bCol state.board) == 0 then
                emptyCell
              else if List.isEmpty hints then
                unfilledCell isSelected bRow bCol
              else
                hintCell isSelected hints bRow bCol
            else
              renderFilledCell isSelected val bRow bCol state
      else
        labelCell right bottom               

renderColsLoop : Int -> Int -> List (Html Msg) -> RenderState -> List (Html Msg)
renderColsLoop row col res state =
  if col >= state.labels.cols then
    List.reverse res
  else
    renderColsLoop
      row
      (col+1)
      (renderCell row col state :: res)
      state
          
renderCols : Int -> RenderState -> List (Html Msg)
renderCols row state =
  renderColsLoop row 0 [] state

renderRow : Int -> RenderState -> Html Msg
renderRow row state =
   tr [] <| renderCols row state

renderRowsLoop : Int -> List (Html Msg) -> RenderState -> List (Html Msg)
renderRowsLoop row res state =
  if row >= state.labels.rows then
    List.reverse res
  else
    renderRowsLoop (row+1)
                   (renderRow row state :: res)
                   state

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

renderRows : GameState -> List (Html Msg)
renderRows state =
  let cellClasses = computeFilledCellClasses state.board state.guesses
      allDone = isAllDone state.board state.guesses
      state2 = makeRenderState state cellClasses allDone
  in
      renderRowsLoop 0 [] state2

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
            in
                List.map (\x -> List.map toString x) run
                  |> List.map String.concat
                  |> String.join " "

rowHelperText : GameState -> String
rowHelperText state =
  helperText (0, 1) (0, -1) snd state

colHelperText : GameState -> String
colHelperText state =
  helperText (1, 0) (-1, 0) fst state

render : GameState -> Html Msg
render state =
  div []
    [ Styles.Board.style
    , table [ class Table ]
        (renderRows state)
    , if state.flags.showPossibilities then
        div [ class Helper ]
          [ text ("row: " ++ (rowHelperText state))
          , br
          , text ("col: " ++ (colHelperText state))
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

keycodeCell : Int -> String -> GameState -> Html Msg
keycodeCell keycode label state =
  td [ class KeypadTd
     , if label == "*" then
         onClick ToggleShowPossibilities
       else if label == "#" then
         onClick ToggleHintInput
       else
         onClickWithInt PressKey keycode
     ]
    [ button [ keypadButtonClass label state
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

renderKeypadCell : Char -> GameState -> Html Msg
renderKeypadCell char state =
  keycodeCell (keypadKeycode char) (String.fromList [char]) state

renderKeypadRow : String -> GameState -> Html Msg
renderKeypadRow string state =
  let chars = String.toList string
  in
      tr []
        <| List.map (\x -> renderKeypadCell x state) chars

-- 1 2 3 ^
-- 4 5 6 v
-- 7 8 9 <
-- * 0 # >
renderKeypad : GameState -> Html Msg
renderKeypad state =
  table [ class Table]
    [ Styles.Board.style
    , renderKeypadRow "123*" state
    , renderKeypadRow "456#" state
    , renderKeypadRow "78^ " state
    , renderKeypadRow "9<v>" state
    ]
