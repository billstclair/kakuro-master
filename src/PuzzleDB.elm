----------------------------------------------------------------------
--
-- PuzzlesDB.elm
-- "Database" of Kakuro puzzles
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module PuzzleDB exposing
  ( boardKinds
  , boardsOfKind
  , numberOfBoardsOfKind
  , getBoardOfKind
  , kindForSpec
  , findBoard
  )

import Puzzles
import Board exposing (Board)

import String
import Char
import Array
import List.Extra as LE

digits : List Char
digits = String.toList "123456789"

specCharToInt : Char -> Int
specCharToInt char =
  if List.member char digits then
    (Char.toCode char) - (Char.toCode '0')
  else
    0

fillBoardFromSpec : Int -> Int -> String -> Board Int -> Board Int
fillBoardFromSpec row kind specTail board =
  if row >= kind then
    board
  else
    let rowArray =
          String.left kind specTail
            |> String.toList
            |> List.map specCharToInt
            |> Array.fromList
        tail = String.dropLeft kind specTail
        newBoard = Board.setRow row rowArray board
    in
        fillBoardFromSpec (row+1) kind tail newBoard

boardFromSpec : Int -> String -> Board Int
boardFromSpec kind spec =
  fillBoardFromSpec 0 kind spec (Board.makeWithSpec kind kind 0 (Just spec))

comparableForSpec : (Int, Int, Int, Int, String) -> (Int, Int, Int, Int)
comparableForSpec (k, v, b, n, s) =
  (k, v, b, n)

sortSpecs : List (Int, Int, Int, Int, String) -> List (Int, Int, Int, Int, String)
sortSpecs specs =
  List.sortBy comparableForSpec specs

log : String -> a -> b -> b
log str a b =
  let x = Debug.log str a
  in
      b

setBoardIndex : Int -> Board Int -> Board Int
setBoardIndex index board =
  { board | index = Just index }

segregatePuzzles : List (Int, Int, Int, Int, String) -> List (Int, List (Board Int)) -> List (Int, List (Board Int))
segregatePuzzles specs res =
  case specs of
      [] ->
        let (kinds, boardss) = List.unzip res
            tupless = List.map
                      (\boards ->
                         List.map2 setBoardIndex
                                   [1..(List.length boards)]
                                   (List.reverse boards))
                      boardss
        in
            LE.zip kinds tupless
      ((k,v,b,n,s) :: tail) ->
        let board = boardFromSpec k s
            pred = (\(k2,bs) -> k2 == k)
            maybePair = LE.find pred res
            newres = case maybePair of
                         Nothing ->
                           (k, [board]) :: res
                         Just (k3, boards) ->
                           LE.replaceIf pred (k, board :: boards) res
        in
            segregatePuzzles tail newres

kindedBoards : List (Int, List ( Board Int))
kindedBoards = segregatePuzzles (sortSpecs Puzzles.puzzles) []

boardKinds : List Int
boardKinds = List.map fst kindedBoards

boardsOfKind : Int -> List (Board Int)
boardsOfKind kind =
  case LE.find (\(k,b) -> k == kind) kindedBoards of
      Nothing -> []
      Just (k, boards) -> boards

numberOfBoardsOfKind : Int -> Int
numberOfBoardsOfKind kind =
  List.length (boardsOfKind kind)

getBoardOfKind : Int -> Int -> Board Int
getBoardOfKind kind index =
  case LE.find (\board -> board.index == (Just index))
               (boardsOfKind kind)
  of
      Nothing ->
        if index == 1 then
          Board.make kind kind 0
        else
          getBoardOfKind kind 1
      Just res ->
        res

kindForSpec : String -> Int
kindForSpec spec =
  case String.length spec of
      100 -> 10
      64 -> 8
      _ -> 6

findBoard : String -> Board Int
findBoard spec =
  let kind = kindForSpec spec
  in
      case LE.find (\board -> board.spec == (Just spec))
                   (boardsOfKind kind)
      of
          Nothing -> getBoardOfKind kind 1
          Just res -> res
