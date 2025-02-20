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
    ( GenerateRowState, generateRowStep
    , cellChoices, fixChoicesForSums
    , eachCell
    , generate, generateChoices, initialGenerateRowState, random, randomChoice, third
    )

{-| Code to generate a new Kakuro board.

@docs GenerateRowState, initialRowState, generateRowStep
@docs cellChoices, fixChoicesForSums
@docs eachCell

-}

import Array exposing (Array)
import Board exposing (Board)
import List.Extra as LE
import Random
import SharedTypes exposing (HintsBoard, IntBoard)


{-| Create a new Board with the given number of rows and columns.

    generate rows cols seed -> ( success, board, nextSeed )

-}
generate : Int -> Int -> Random.Seed -> ( Bool, IntBoard, Random.Seed )
generate rows cols seed =
    let
        state =
            initialGenerateRowState rows cols
    in
    generateInternal state seed


generateInternal : GenerateRowState -> Random.Seed -> ( Bool, IntBoard, Random.Seed )
generateInternal state seed =
    let
        ( newState, newSeed ) =
            generateRowStep state seed
    in
    if newState.done then
        ( newState.success, newState.board, newSeed )

    else
        generateInternal newState newSeed



-- This gives (expt 3 10) = 59049 total row tries,
-- for a 10x10 board.


maxGenerateRowTries : Int
maxGenerateRowTries =
    3


initialGenerateRowState : Int -> Int -> GenerateRowState
initialGenerateRowState rows cols =
    let
        board =
            Board.make rows cols 0
    in
    { done = False
    , success = True
    , row = 0
    , col = 0
    , board = board
    , tries = 0
    , rowStack = []
    , colState = initialGenerateColState board
    }


type alias GenerateRowState =
    { done : Bool --True if all done
    , success : Bool --False if the call to generateRowStep failed
    , row : Int -- row & column for board
    , col : Int
    , board : IntBoard
    , tries : Int --tries left in the current row
    , rowStack : List ( Int, GenerateColState ) --for each row < row, (tries, colState)
    , colState : GenerateColState
    }


{-| Compute the choices for the given cell that don't collide with existing
values in its row and column with smaller indices.
-}
cellChoices : Int -> Int -> IntBoard -> List Int
cellChoices row col board =
    let
        rows =
            board.rows

        cols =
            board.cols

        maxRow =
            rows - 1

        maxCol =
            cols - 1

        get r c =
            Board.get r c board

        choices =
            [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

        colLoop : Int -> List Int -> List Int
        colLoop c ch =
            if c < 0 then
                ch

            else
                let
                    x =
                        get row c
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
                        get r col
                in
                if x == 0 then
                    ch

                else
                    rowLoop (r - 1) <| LE.remove x ch

        maybeRemove0 : List Int -> List Int
        maybeRemove0 ch =
            let
                tooCloseToEdge =
                    ((row == 1) && get 0 col /= 0)
                        || ((col == 1) && get row (col - 1) /= 0)
            in
            if tooCloseToEdge then
                LE.remove 0 ch

            else
                let
                    tooCloseTo0 =
                        ((row >= rows - 2)
                            && ((row > 0 && get (row - 1) col == 0)
                                    || (row > 1 && (get (row - 2) col == 0))
                               )
                        )
                            || ((col >= cols - 2)
                                    && (col > 0 && (get row (col - 1) == 0))
                                    || (col > 1 && (get row (col - 2) == 0))
                               )
                in
                if tooCloseTo0 then
                    LE.remove 0 ch

                else
                    let
                        snakeTailAgainstWall =
                            ((row == 0 || get (row - 1) col == 0)
                                && ((row == 1 || (row > 1 && get (row - 2) col == 0))
                                        && (get (row - 1) col /= 0)
                                   )
                            )
                                || ((col == 0 || get row (col - 1) == 0)
                                        && ((col == 1 || (col > 1 && get row (col - 2) == 0))
                                                && (get row (col - 1) /= 0)
                                           )
                                   )
                    in
                    if snakeTailAgainstWall then
                        LE.remove 0 ch

                    else
                        ch

        maybeOnly0Ch =
            if
                (row == maxRow && get (row - 1) col == 0)
                    || (col == maxCol && get row (col - 1) == 0)
            then
                -- Don't allow single-wide row or column segments on bottom or right
                [ 0 ]

            else
                choices
    in
    maybeRemove0 maybeOnly0Ch
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


{-| I originally thought this would help, but that's because the game
already has code to compute the possibilities, or so I thought.
The game has code to compute the possbilities knowing the sum.
The sum isn't known here.
I've kept the `identity` function, and call it in `TestGenerate.elm`,
but don't intend to ever implement it.
-}
fixChoicesForSumsInternal : IntBoard -> HintsBoard -> HintsBoard
fixChoicesForSumsInternal board choicesBoard =
    choicesBoard


{-| Call the function on each cell of the board.
-}
eachCell : (Int -> Int -> Board a -> Board a) -> Board a -> Board a
eachCell f board =
    let
        rowRange =
            List.range 0 (board.rows - 1)

        colRange =
            List.range 0 (board.cols - 1)

        eachRow : Int -> Board a -> Board a
        eachRow row b =
            List.foldl (eachCol row) b colRange

        eachCol : Int -> Int -> Board a -> Board a
        eachCol row col b =
            f row col b
    in
    List.foldl eachRow board rowRange


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

        choicesBoard =
            SharedTypes.emptyHintsBoard rows cols

        setCell : Int -> Int -> HintsBoard -> HintsBoard
        setCell row col chb =
            let
                choices =
                    cellChoices row col board
            in
            Board.set row col choices chb
    in
    eachCell setCell choicesBoard
        |> fixChoicesForSums board


generateRowStep : GenerateRowState -> Random.Seed -> ( GenerateRowState, Random.Seed )
generateRowStep rowState seed =
    generateRowStepInternal True rowState seed


generateRowStepInternal : Bool -> GenerateRowState -> Random.Seed -> ( GenerateRowState, Random.Seed )
generateRowStepInternal newCol rowState seed =
    let
        { done, row, col, board, tries, rowStack, colState } =
            rowState
    in
    if done then
        ( rowState, seed )

    else
        let
            ( nextColState, nextSeed ) =
                generateColStep newCol colState seed
        in
        if nextColState.success then
            let
                nextRowState =
                    { rowState
                        | col = nextColState.col
                        , board = nextColState.board
                        , colState = nextColState
                    }
            in
            if nextRowState.col < board.cols then
                ( nextRowState
                , nextSeed
                )

            else
                let
                    nextRow =
                        row + 1
                in
                if nextRow >= board.rows then
                    ( { nextRowState | done = True }
                    , nextSeed
                    )

                else
                    let
                        ncs =
                            { colState
                                | row = nextRow
                                , col = -1
                                , colStack = []
                            }
                    in
                    generateRowStep
                        { rowState
                            | row = nextRow
                            , col = -1
                            , colState = ncs
                            , tries = 1
                            , rowStack = ( tries, colState ) :: rowStack
                        }
                        nextSeed

        else if tries < maxGenerateRowTries then
            -- Try this row again
            let
                ncs =
                    { colState
                        | col = -1
                        , possibilities = []
                        , colStack = []
                        , success = True
                    }
            in
            generateRowStep
                { rowState
                    | col = -1
                    , tries = tries + 1
                    , colState = ncs
                }
                seed

        else
            -- Back up a row
            case rowStack of
                [] ->
                    ( { rowState
                        | done = True
                        , success = False
                      }
                    , nextSeed
                    )

                ( lastTries, lastColState ) :: tail ->
                    if lastTries >= maxGenerateRowTries then
                        ( { rowState
                            | done = True
                            , success = False
                          }
                        , nextSeed
                        )

                    else
                        generateRowStep
                            { rowState
                                | row = row - 1
                                , col = lastColState.col
                                , board = lastColState.board
                                , tries = lastTries + 1
                                , colState = lastColState
                                , rowStack = tail
                            }
                            nextSeed


initialGenerateColState : IntBoard -> GenerateColState
initialGenerateColState board =
    { success = True
    , row = 0
    , col = -1
    , board = board
    , possibilities = []
    , colStack = []
    }


type alias GenerateColState =
    { success : Bool --False if the call to generateColState failed.
    , row : Int --row and column for board
    , col : Int --this code never changes these
    , board : IntBoard
    , possibilities : List Int --remaining possibilities for this column
    , colStack : List ( IntBoard, List Int, Bool ) --for each col < col: (board, possibilities, lastColOnlyZero)
    }


generateColStep : Bool -> GenerateColState -> Random.Seed -> ( GenerateColState, Random.Seed )
generateColStep newCol state seed =
    let
        { success, row, col, board, possibilities, colStack } =
            state
    in
    if not success then
        ( state, seed )

    else if col >= board.cols then
        ( state, seed )

    else
        let
            ( realCol, realPossibilities, newStack ) =
                if newCol then
                    let
                        choices =
                            cellChoices row (col + 1) board

                        just0 =
                            choices == [ 0 ]
                    in
                    ( col + 1
                    , choices
                    , if col < 0 then
                        []

                      else
                        ( board, possibilities, just0 ) :: colStack
                    )

                else
                    ( col, possibilities, colStack )
        in
        let
            ( maybeVal, newPossibilities, newSeed ) =
                case realPossibilities of
                    [] ->
                        ( Nothing, realPossibilities, seed )
                            |> (\x ->
                                    let
                                        n =
                                            Debug.log "no possibilities, (row, col, seed)"
                                                ( row, realCol, seed )
                                    in
                                    x
                               )

                    [ onePossibility ] ->
                        ( Just onePossibility, [], seed )
                            |> (\x ->
                                    let
                                        n =
                                            Debug.log "onePossibility (x, row, col)"
                                                ( x, row, realCol )
                                    in
                                    x
                               )

                    _ :: _ ->
                        randomChoice realPossibilities seed
        in
        case maybeVal of
            Just val ->
                ( { state
                    | col = realCol
                    , possibilities = newPossibilities
                    , board = Board.set row realCol val board
                    , colStack = newStack
                  }
                , newSeed
                )

            Nothing ->
                -- No result. Back up
                if realCol <= 0 then
                    -- We've backed up all we can. Fail.
                    ( { state | success = False }
                    , newSeed
                    )

                else
                    -- Back up
                    let
                        lastCol =
                            realCol - 1
                    in
                    case newStack of
                        [] ->
                            -- Shouldn't happen
                            ( { state | success = False }, seed )

                        ( lastBoard, lastPossibilities, only0 ) :: tail ->
                            if only0 then
                                ( { state | success = False }
                                , seed
                                )

                            else
                                generateColStep
                                    False
                                    { state
                                        | col = lastCol
                                        , success = True
                                        , board = lastBoard
                                        , possibilities = lastPossibilities
                                        , colStack = tail
                                    }
                                    seed


random : Int -> Int -> Random.Seed -> ( Int, Random.Seed )
random min max seed =
    Random.step (Random.int min max) seed


randomChoice : List a -> Random.Seed -> ( Maybe a, List a, Random.Seed )
randomChoice choices seed =
    if choices == [] then
        ( Nothing, [], seed )

    else
        let
            ( idx, nextSeed ) =
                random 0 (List.length choices - 1) seed

            ( head, tail ) =
                LE.splitAt idx choices
        in
        case tail of
            [] ->
                -- Can't happen
                ( Nothing, choices, nextSeed )

            a :: rest ->
                ( Just a, head ++ rest, nextSeed )



-- For debugging


third : ( a, b, c ) -> c
third ( a, b, c ) =
    c
