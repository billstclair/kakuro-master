----------------------------------------------------------------------
--
-- Generate.elm
-- Generate new Kakuro board layout.
-- Copyright (c) 2016-2025 Bill St. Clair <billstclair@gmail.com>
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
    , random, randomChoice, third
    )

{-| Code to generate a new Kakuro board.

@docs generate

-}

import Array exposing (Array)
import Board exposing (Board)
import List.Extra as LE
import Random


{-| Create a new Board with the given number of rows and columns.

    generate rows cols seed -> ( success, board, nextSeed )

-}
generate : Int -> Int -> Random.Seed -> ( Bool, Board Int, Random.Seed )
generate rows cols seed =
    Board.make rows cols 0
        |> generateRows 0 0 seed


maxRowTries : Int
maxRowTries =
    10


generateRows : Int -> Int -> Random.Seed -> Board Int -> ( Bool, Board Int, Random.Seed )
generateRows tries startRow seed board =
    if startRow >= board.rows then
        ( True, board, seed )

    else
        let
            ( success, nextBoard, nextSeed ) =
                generateColumns 0 (Debug.log "generateRows" startRow) 0 seed board
        in
        if success then
            generateRows 0 (startRow + 1) nextSeed nextBoard

        else if tries >= maxRowTries then
            if startRow <= 0 then
                ( False, board, nextSeed )

            else
                generateRows 0 (startRow - 1) nextSeed board

        else
            generateRows (tries + 1) startRow nextSeed board


maxColTries : Int
maxColTries =
    10


generateColumns : Int -> Int -> Int -> Random.Seed -> Board Int -> ( Bool, Board Int, Random.Seed )
generateColumns tries row startCol seed board =
    if startCol >= board.cols then
        ( True, board, seed )

    else
        let
            ( success, nextBoard, nextSeed ) =
                generateColumn row (Debug.log "  generateColumn" startCol) seed board
        in
        if success then
            generateColumns 0 row (startCol + 1) nextSeed nextBoard

        else if tries >= maxColTries then
            if startCol <= 0 then
                ( False, board, nextSeed )

            else
                generateColumns 0 row (startCol - 1) nextSeed board

        else
            generateColumns (tries + 1) row startCol nextSeed board


generateColumn : Int -> Int -> Random.Seed -> Board Int -> ( Bool, Board Int, Random.Seed )
generateColumn row col seed board =
    -- TODO
    ( True, board, seed )


random : Int -> Int -> Random.Seed -> ( Int, Random.Seed )
random min max seed =
    Random.step (Random.int min max) seed


randomChoice : List a -> Random.Seed -> ( Maybe a, List a, Random.Seed )
randomChoice choices seed =
    let
        ( idx, nextSeed ) =
            random 0 (List.length choices - 1) seed

        ( head, tail ) =
            LE.splitAt idx choices
    in
    case tail of
        [] ->
            -- Only happens when `choices` is empty
            ( Nothing, choices, nextSeed )

        a :: rest ->
            ( Just a, head ++ rest, nextSeed )



-- For debugging


third : ( a, b, c ) -> c
third ( a, b, c ) =
    c
