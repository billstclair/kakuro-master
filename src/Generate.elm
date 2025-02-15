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
generate : Int -> Int -> Random.Seed -> ( Board Int, Random.Seed )
generate rows cols seed =
    Board.make rows cols 0
        |> generateRows 0 0 seed


generateRows : Int -> Int -> Random.Seed -> Board Int -> ( Board Int, Random.Seed )
generateRows tries startRow seed board =
    if startRow >= board.rows then
        ( board, seed )

    else
        let
            col0Choices =
                columnChoices startRow 0 board

            ( nextBoard, nextSeed ) =
                generateColumns (Debug.log "generateRows" startRow)
                    0
                    col0Choices
                    [ [] ]
                    seed
                    board
        in
        generateRows 0 (startRow + 1) nextSeed nextBoard


columnChoices : Int -> Int -> Board Int -> List Int
columnChoices row col board =
    -- TODO
    []


generateColumns : Int -> Int -> List Int -> List (List Int) -> Random.Seed -> Board Int -> ( Board Int, Random.Seed )
generateColumns row startCol choices prevChoicess seed board =
    if startCol >= board.cols then
        ( board, seed )

    else
        let
            ( success, ( nextChoices, nextSeed, nextBoard ) ) =
                generateColumn row
                    (Debug.log "  generateColumn" startCol)
                    choices
                    seed
                    board
        in
        if success then
            let
                nextCol =
                    startCol + 1

                nextColChoices =
                    columnChoices row nextCol board
            in
            generateColumns row
                nextCol
                nextColChoices
                (choices :: prevChoicess)
                nextSeed
                nextBoard

        else if not <| List.isEmpty nextChoices then
            generateColumns row startCol nextChoices prevChoicess nextSeed board

        else if startCol > 0 then
            case prevChoicess of
                [] ->
                    -- Can't happen, unless there really aren't any solutions
                    ( board, nextSeed )

                prevChoices :: prevPrevChoicess ->
                    generateColumns row
                        (startCol - 1)
                        prevChoices
                        prevPrevChoicess
                        nextSeed
                        board

        else
            -- Can't happen, unless there really aren't any solutions
            ( board, nextSeed )


generateColumn : Int -> Int -> List Int -> Random.Seed -> Board Int -> ( Bool, ( List Int, Random.Seed, Board Int ) )
generateColumn row col choices seed board =
    -- TODO
    ( True, ( choices, seed, board ) )


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
