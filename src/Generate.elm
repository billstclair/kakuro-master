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
    ( generate, generateChoices, cellChoices, fixChoicesForSums
    , random, randomChoice, third
    )

{-| Code to generate a new Kakuro board.

@docs generate, generateChoices, cellChoices, fixChoicesForSums

-}

import Array exposing (Array)
import Board exposing (Board)
import List.Extra as LE
import Random
import SharedTypes exposing (HintsBoard, IntBoard)


{-| Create a new Board with the given number of rows and columns.

    generate rows cols seed -> ( success, board, nextSeed )

-}
generate : Int -> Int -> Random.Seed -> ( IntBoard, Random.Seed )
generate rows cols seed =
    Board.make rows cols 0
        |> generateRows 0 0 seed


generateRows : Int -> Int -> Random.Seed -> IntBoard -> ( IntBoard, Random.Seed )
generateRows tries startRow seed board =
    if startRow >= board.rows then
        ( board, seed )

    else
        let
            col0Choices =
                cellChoices startRow 0 board

            ( nextBoard, nextSeed ) =
                generateColumns (Debug.log "generateRows" startRow)
                    0
                    col0Choices
                    [ [] ]
                    seed
                    board
        in
        generateRows 0 (startRow + 1) nextSeed nextBoard


{-| Compute the choices for the given cell that don't collide with existing
values in its row and column with smaller indices.
-}
cellChoices : Int -> Int -> IntBoard -> List Int
cellChoices row col board =
    let
        choices =
            [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

        colLoop : Int -> List Int -> List Int
        colLoop c ch =
            if c < 0 then
                ch

            else
                let
                    x =
                        Board.get row c board
                in
                if x == 0 then
                    ch

                else
                    colLoop (c - 1) <| LE.remove x ch

        rowLoop : Int -> List Int -> List Int
        rowLoop r ch =
            if r < 0 then
                ch

            else
                let
                    x =
                        Board.get r col board
                in
                if x == 0 then
                    ch

                else
                    rowLoop (r - 1) <| LE.remove x ch

        maybeRemove0 : List Int -> List Int
        maybeRemove0 ch =
            let
                tooCloseToEdge =
                    (row == 1)
                        || (row == board.rows - 2)
                        || (col == 1)
                        || (col == board.cols - 2)
            in
            if tooCloseToEdge then
                LE.remove 0 ch

            else
                let
                    tooCloseTo0 =
                        (row > 0 && Board.get (row - 1) col board == 0)
                            || (row > 1 && (Board.get (row - 2) col board == 0))
                            || ((row < board.rows - 1)
                                    && (Board.get (row + 1) col board == 0)
                               )
                            || ((row < board.rows - 2)
                                    && (Board.get (row + 2) col board == 0)
                               )
                            || (col > 0 && (Board.get row (col - 1) board == 0))
                            || (col > 1 && (Board.get row (col - 2) board == 0))
                            || ((col < board.cols - 1)
                                    && (Board.get row (col + 1) board == 0)
                               )
                            || ((col < board.cols - 2)
                                    && (Board.get row (col + 2) board == 0)
                               )
                in
                if tooCloseTo0 then
                    LE.remove 0 ch

                else
                    ch
    in
    maybeRemove0 choices
        |> colLoop (col - 1)
        |> rowLoop (row - 1)


{-| Call this after collecting results for calling `cellChoices`
for all cells, and collecting them in `choicesBoard`.

Removes those choices from each cell that can't be part of the sum for the
row and column.

-}
fixChoicesForSums : IntBoard -> HintsBoard -> HintsBoard
fixChoicesForSums board choicesBoard =
    let
        fixLoop : HintsBoard -> HintsBoard
        fixLoop chb =
            let
                chb2 =
                    fixChoicesForSumsInternal board chb
            in
            if chb2 == chb then
                chb

            else
                -- Loop until it doesn't change
                fixLoop chb2
    in
    fixLoop choicesBoard


fixChoicesForSumsInternal : IntBoard -> HintsBoard -> HintsBoard
fixChoicesForSumsInternal board choicesBoard =
    -- TODO
    choicesBoard


{-| Call cellChoices for each cell, collecting them in a `HintsBoard`.
Then call fixChoicesForSums to remove choices that can't be part of
a row and column sum
-}
generateChoices : IntBoard -> HintsBoard
generateChoices board =
    let
        rows =
            board.rows

        cols =
            board.cols

        rowRange =
            List.range 0 (rows - 1)

        colRange =
            List.range 0 (cols - 1)

        choicesBoard =
            SharedTypes.emptyHintsBoard rows cols

        eachRow : Int -> HintsBoard -> HintsBoard
        eachRow row chb =
            List.foldl (eachCol row) chb colRange

        eachCol : Int -> Int -> HintsBoard -> HintsBoard
        eachCol row col chb =
            let
                choices =
                    cellChoices row col board
            in
            Board.set row col choices chb
    in
    List.foldl eachRow choicesBoard rowRange
        |> fixChoicesForSums board


generateColumns : Int -> Int -> List Int -> List (List Int) -> Random.Seed -> IntBoard -> ( IntBoard, Random.Seed )
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
                    cellChoices row nextCol board
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


generateColumn : Int -> Int -> List Int -> Random.Seed -> IntBoard -> ( Bool, ( List Int, Random.Seed, IntBoard ) )
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
