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
    ( generate, generateChoices, cellChoices, fixChoicesForSums, eachCell
    , random, randomChoice, third
    )

{-| Code to generate a new Kakuro board.

@docs generate, generateChoices, cellChoices, fixChoicesForSums, eachCell

-}

import Array exposing (Array)
import Board exposing (Board)
import List.Extra as LE
import Random
import SharedTypes exposing (HintsBoard, IntBoard)


{-| Create a new Board with the given number of rows and columns.

    generate rows cols seed -> ( success, board, nextSeed )

-}
generate : Int -> Int -> Random.Seed -> ( Bool, ( Random.Seed, List ( Int, IntBoard ), IntBoard ) )
generate rows cols seed =
    Board.make rows cols 0
        |> generateRows 0 0 seed []


maxGenerateRowTries : Int
maxGenerateRowTries =
    10


type alias GenerateRowState =
    { done : Bool --True if all done
    , success : Bool --False if the call to generateRowStep failed
    , row : Int -- row & column for board
    , col : Int
    , board : IntBoard
    , tries : Int --tries left in the current row
    , rowStack : List ( Int, GenerateColState ) --for each row < row, (tries, colState)
    , colState : GenerateColState
    , seed : Random.Seed
    }


generateRowStep : GenerateRowState -> GenerateRowState
generateRowStep rowState =
    let
        { done, row, col, board, tries, rowStack, colState, seed } =
            rowState
    in
    if done then
        rowState

    else
        let
            nextCol =
                rowState.col + 1

            ( nextRowState, nextColState ) =
                if nextCol < board.cols then
                    ( { rowState
                        | col = nextCol
                      }
                    , { colState
                        | col = nextCol
                        , possibilities = cellChoices row nextCol board
                        , colStack =
                            ( colState.board, colState.possibilities ) :: colState.colStack
                      }
                    )

                else
                    let
                        nextRow =
                            rowState.row + 1
                    in
                    if nextRow >= board.rows then
                        ( { rowState
                            | done = True
                            , success = True
                          }
                        , colState
                        )

                    else
                        let
                            ncs =
                                { colState
                                    | row = nextRow
                                    , col = 0
                                    , possibilities =
                                        cellChoices nextRow 0 board
                                    , colStack =
                                        ( colState.board, colState.possibilities )
                                            :: colState.colStack
                                }
                        in
                        ( { rowState
                            | row = nextRow
                            , col = 0
                            , colState = ncs
                            , rowStack = ( tries, colState ) :: rowStack
                          }
                        , ncs
                        )
        in
        if nextRowState.done then
            nextRowState

        else
            let
                ( nextNextColState, nextSeed ) =
                    generateColumnStep
                        { nextColState | done = False }
                        seed
            in
            if nextNextColState.success then
                { rowState
                    | success = True
                    , board = nextNextColState.board
                    , colState = nextNextColState
                    , seed = nextSeed
                }

            else if tries < maxGenerateRowTries then
                -- Retry the row
                let
                    oldBoard =
                        case nextColState.colStack |> List.reverse |> List.head of
                            Nothing ->
                                nextColState.board

                            Just ( b, _ ) ->
                                b
                in
                { nextRowState
                    | tries = tries + 1
                    , success = True
                    , board = oldBoard
                    , col = -1
                    , colState =
                        { nextColState
                            | col = -1
                            , possibilities = []
                            , colStack = []
                        }
                }

            else if nextRowState.row > 0 then
                -- Resume the previous row
                case rowStack of
                    [] ->
                        -- Not possbile
                        { nextRowState
                            | done = True
                            , success = False
                        }

                    ( lastTries, lastColState ) :: tail ->
                        { nextRowState
                            | success = True
                            , row = lastColState.row
                            , col = lastColState.col
                            , colState = { lastColState | done = False }
                        }

            else
                { rowState
                    | done = True
                    , success = False
                    , seed = nextSeed
                }


generateRows : Int -> Int -> Random.Seed -> List ( Int, IntBoard ) -> IntBoard -> ( Bool, ( Random.Seed, List ( Int, IntBoard ), IntBoard ) )
generateRows tries startRow seed stack board =
    if startRow >= board.rows then
        ( True, ( seed, [], board ) )

    else if tries >= maxGenerateRowTries then
        case stack of
            [] ->
                ( False, ( seed, [], board ) )

            ( _, lastBoard ) :: lastStack ->
                ( False, ( seed, lastStack, lastBoard ) )

    else
        let
            col0Choices =
                cellChoices startRow 0 board

            ( colSuccess, nextBoard, nextSeed ) =
                generateColumns (Debug.log "generateRows" startRow)
                    0
                    col0Choices
                    []
                    seed
                    board
        in
        if colSuccess then
            let
                nextStack =
                    ( tries, nextBoard ) :: stack

                ( rowSuccess, ( nextNextSeed, lastStack, nextNextBoard ) ) =
                    generateRows 0 (startRow + 1) nextSeed nextStack nextBoard
            in
            if rowSuccess then
                ( True, ( nextNextSeed, stack, nextNextBoard ) )

            else
                generateRows (tries + 1) startRow nextNextSeed stack board

        else
            case stack of
                [] ->
                    ( False, ( nextSeed, [], board ) )

                ( lastTries, lastBoard ) :: lastStack ->
                    generateRows lastTries
                        (startRow - 1)
                        nextSeed
                        lastStack
                        lastBoard


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
                            || (col > 0 && (Board.get row (col - 1) board == 0))
                            || (col > 1 && (Board.get row (col - 2) board == 0))
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


type alias GenerateColState =
    { done : Bool -- True if nothing left to do.
    , success : Bool --False if the call to generateColState failed.
    , row : Int --row and column for board
    , col : Int --this code never changes these
    , board : IntBoard
    , possibilities : List Int --remaining possibilities for this column
    , colStack : List ( IntBoard, List Int ) --for each col < col: (board, possibilities)
    }


generateColumnStep : GenerateColState -> Random.Seed -> ( GenerateColState, Random.Seed )
generateColumnStep state seed =
    let
        { done, row, col, board, possibilities } =
            state
    in
    if done then
        ( state, seed )

    else
        -- TODO
        ( { state | done = True }, seed )


generateColumns : Int -> Int -> List Int -> List (List Int) -> Random.Seed -> IntBoard -> ( Bool, IntBoard, Random.Seed )
generateColumns row startCol choices prevChoicess seed board =
    if startCol >= board.cols then
        ( True, board, seed )

    else
        let
            ( success, ( nextChoices, nextBoard, nextSeed ) ) =
                generateColumn row
                    startCol
                    choices
                    seed
                    board
        in
        if success then
            let
                nextCol =
                    startCol + 1

                nextColChoices =
                    cellChoices row nextCol nextBoard
            in
            generateColumns row
                nextCol
                nextColChoices
                (nextChoices :: prevChoicess)
                nextSeed
                nextBoard

        else if nextChoices /= [] then
            generateColumns row startCol nextChoices prevChoicess nextSeed board

        else if startCol > 0 then
            case prevChoicess of
                [] ->
                    ( False, board, nextSeed )

                prevChoices :: prevPrevChoicess ->
                    generateColumns row
                        (startCol - 1)
                        prevChoices
                        prevPrevChoicess
                        nextSeed
                        board

        else
            ( False, board, nextSeed )


generateColumn : Int -> Int -> List Int -> Random.Seed -> IntBoard -> ( Bool, ( List Int, IntBoard, Random.Seed ) )
generateColumn row col choices seed board =
    if choices == [] then
        ( False, ( choices, board, seed ) )

    else
        let
            ( maybeChoice, newChoices, newSeed ) =
                randomChoice choices seed
        in
        case maybeChoice of
            Nothing ->
                ( False, ( newChoices, board, newSeed ) )

            Just choice ->
                let
                    newBoard =
                        Board.set row col choice board
                in
                ( True, ( newChoices, newBoard, newSeed ) )


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
