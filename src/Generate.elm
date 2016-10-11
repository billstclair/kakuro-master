----------------------------------------------------------------------
--
-- Generate.elm
-- Generate new Kakuro board layout.
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
-- This is just a bare bones start, and I decided not to pursue it,
-- using instead precomputed layouts in Puzzles.elm.
--
----------------------------------------------------------------------

module Generate exposing
  ( generate
  )

{-| Code to generate a new Kakuro board.

@docs generate
-}

import Board exposing (Board)

import Array exposing (Array)
import Random

{-| Create a new Board with the same shape as the input Board.

    generate maxrun seed board
-}
generate : Int -> Random.Seed -> Board -> (Board, Random.Seed)
generate maxrun seed board =
  Board.make board.rows board.cols
  |> generateRows 0 maxrun (skip maxrun board) seed

random : Int -> Int -> Random.Seed -> (Int, Random.Seed)
random min max seed =
  Random.step (Random.int min max) seed

skip : Int -> Board -> Int
skip maxrun board =
  if board.cols > 2*maxrun then
    max 1 (board.cols//2 - maxrun)
  else
    max 1 (board.cols - maxrun)

generateRows : Int -> Int -> Int -> Random.Seed -> Board -> (Board, Random.Seed)
generateRows row maxrun maxSkip seed board =
  let (start, seed) = random 0 maxSkip seed
      (rowArray, seed2) = generateRuns start maxrun maxSkip seed
                          (Board.getRow row board)
      brd = Board.setRow row rowArray board
  in
      if row >= (board.rows-1) then
        (brd, seed)
      else
        generateRows (row+1) maxrun maxSkip seed brd

generateRuns : Int -> Int -> Int -> Random.Seed -> (Array Int) ->
               ((Array Int), Random.Seed)
generateRuns start maxrun maxSkip seed rowArray =
  let len = Array.length rowArray
      maxmaxrun = min maxrun <| len-start
  in
      if maxmaxrun < 2 then
        (rowArray, seed)
      else
        let (run, seed2) = random 2 maxmaxrun seed
            (ra, seed3) = generateRun run start seed2 rowArray
            ms = max 1 <| min maxSkip <| len - start - run - 3
            (sk, seed4) = random 1 ms seed3
            start2 = start + run + sk
        in
            generateRuns start2 maxrun maxSkip seed4 ra

generateRun : Int -> Int -> Random.Seed -> (Array Int) -> ((Array Int), Random.Seed)
generateRun run start seed rowArray =
  let ra = Array.set start run rowArray
  in
      (ra, seed)
