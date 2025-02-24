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
    , rowStack = []
    , colState = initialGenerateColState board
    }


type alias GenerateRowState =
    { done : Bool --True if all done
    , success : Bool --False if the call to generateRowStep failed
    , row : Int -- row & column for board
    , col : Int
    , board : IntBoard
    , rowStack : List GenerateColState --for each row < row: colState
    , colState : GenerateColState
    }


{-| Compute the choices for the given cell that don't collide with existing
values in its row and column with smaller indices.
-}
cellChoices : Int -> Int -> IntBoard -> Random.Seed -> ( List Int, Random.Seed )
cellChoices row col board seed =
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
                                || ((row == maxRow)
                                        && (col == maxCol - 1)
                                        && (get (row - 2) (col + 1) == 0)
                                   )
                                || ((col == 0 || get row (col - 1) == 0)
                                        && ((col == 1 || (col > 1 && get row (col - 2) == 0))
                                                && (get row (col - 1) /= 0)
                                           )
                                        || ((col == maxCol)
                                                && (row == maxRow - 1)
                                                && (get (row + 1) (col - 2) == 0)
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

        limitWholeLines : List Int -> Random.Seed -> ( List Int, Random.Seed )
        limitWholeLines ch sd =
            if ch == [ 0 ] || (not <| List.member 0 ch) then
                ( ch, sd )

            else
                let
                    ( fullRows, fullCols ) =
                        Debug.log
                            ("getFullRowsAndCols, ("
                                ++ String.fromInt row
                                ++ ","
                                ++ String.fromInt col
                                ++ ")"
                            )
                        <|
                            getFullRowsAndCols row col board

                    ( rowCh, sd2 ) =
                        if fullRows < maxFullRowsOrCols then
                            ( ch, sd )

                        else
                            let
                                ( zeropr, sd3 ) =
                                    random 0 (board.rows - row - 1) sd
                            in
                            if zeropr == 0 then
                                ( [ 0 ], sd3 )

                            else
                                ( ch, sd )
                in
                if rowCh == [ 0 ] || fullCols < maxFullRowsOrCols then
                    ( rowCh, sd )

                else
                    let
                        ( zeropc, sd4 ) =
                            random 0 (board.rows - row - 1) sd2
                    in
                    if zeropc == 0 then
                        ( [ 0 ], sd4 )

                    else
                        ( ch, sd4 )
    in
    let
        ( ch2, sd2 ) =
            limitWholeLines maybeOnly0Ch seed
    in
    if ch2 == [ 0 ] then
        ( ch2, sd2 )

    else
        ( maybeRemove0 ch2
            |> colLoop (col - 1)
            |> rowLoop (row - 1)
        , sd2
        )


maxFullRowsOrCols : Int
maxFullRowsOrCols =
    2


getFullRowsAndCols : Int -> Int -> IntBoard -> ( Int, Int )
getFullRowsAndCols row col board =
    let
        get r c =
            Board.get r c board

        maxRowCnt =
            min board.cols 9

        maxColCnt =
            min board.rows 9

        fullRows =
            let
                eachRow r rowcnt =
                    let
                        eachCol c colcnt =
                            if c < 0 then
                                colcnt

                            else if get r c == 0 then
                                eachCol (c - 1) colcnt

                            else
                                eachCol (c - 1) <| colcnt + 1

                        startCol =
                            if r < row then
                                board.cols - 1

                            else
                                col
                    in
                    if r < 0 || rowcnt >= maxRowCnt then
                        rowcnt

                    else if eachCol startCol 0 >= maxRowCnt then
                        eachRow (r - 1) <| rowcnt + 1

                    else
                        eachRow (r - 1) rowcnt
            in
            eachRow (row - 1) 0

        fullCols =
            let
                eachCol c colcnt =
                    let
                        eachRow r rowcnt =
                            if r < 0 then
                                rowcnt

                            else if get r c == 0 then
                                eachRow (r - 1) rowcnt

                            else
                                eachRow (r - 1) <| rowcnt + 1

                        startRow =
                            if c < col then
                                board.rows - 1

                            else
                                row
                    in
                    if c < 0 || colcnt >= maxColCnt then
                        colcnt

                    else if eachRow startRow 0 >= maxColCnt then
                        eachCol (c - 1) <| colcnt + 1

                    else
                        eachCol (c - 1) colcnt
            in
            eachCol (col - 1) 0
    in
    ( fullRows, fullCols )


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


{-| Call the function with a seed on each cell of the board.
-}
eachCellWithSeed : (Int -> Int -> Board a -> Random.Seed -> ( Board a, Random.Seed )) -> Board a -> Random.Seed -> ( Board a, Random.Seed )
eachCellWithSeed f board seed =
    let
        rowRange =
            List.range 0 (board.rows - 1)

        colRange =
            List.range 0 (board.cols - 1)

        eachRow : Int -> ( Board a, Random.Seed ) -> ( Board a, Random.Seed )
        eachRow row ( b, sd ) =
            List.foldl (eachCol row) ( b, sd ) colRange

        eachCol : Int -> Int -> ( Board a, Random.Seed ) -> ( Board a, Random.Seed )
        eachCol row col ( b, sd ) =
            f row col b sd
    in
    List.foldl eachRow ( board, seed ) rowRange


{-| Call cellChoices for each cell, collecting them in a `HintsBoard`.
Then call fixChoicesForSums to remove choices that can't be part of
a row and column sum
-}
generateChoices : IntBoard -> Random.Seed -> ( HintsBoard, Random.Seed )
generateChoices board seed =
    let
        rows =
            board.rows

        cols =
            board.cols

        choicesBoard =
            SharedTypes.emptyHintsBoard rows cols

        setCell : Int -> Int -> HintsBoard -> Random.Seed -> ( HintsBoard, Random.Seed )
        setCell row col chb sd =
            let
                ( choices, sd2 ) =
                    cellChoices row col board seed
            in
            ( Board.set row col choices chb, sd2 )

        ( choicesBoard2, seed2 ) =
            eachCellWithSeed setCell choicesBoard seed
    in
    ( fixChoicesForSums board choicesBoard2, seed2 )


generateRowStep : GenerateRowState -> Random.Seed -> ( GenerateRowState, Random.Seed )
generateRowStep rowState seed =
    generateRowStepInternal True rowState seed


generateRowStepInternal : Bool -> GenerateRowState -> Random.Seed -> ( GenerateRowState, Random.Seed )
generateRowStepInternal newCol rowState seed =
    let
        { done, row, col, board, rowStack, colState } =
            rowState
    in
    if done then
        ( rowState, seed )

    else
        let
            ( backup, nextColState, nextSeed ) =
                let
                    ( ncs, ns ) =
                        generateColStep newCol colState seed
                in
                ( not ncs.success, ncs, ns )
        in
        if not backup then
            let
                nextRowState =
                    { rowState
                        | col = nextColState.col
                        , board = nextColState.board
                        , colState = nextColState
                    }
            in
            if nextRowState.col < board.cols then
                ( nextRowState, nextSeed )

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
                                , possibilities = []
                                , colStack = []
                            }
                    in
                    generateRowStep
                        { rowState
                            | row = nextRow
                            , col = -1
                            , colState = ncs
                            , rowStack = colState :: rowStack
                        }
                        nextSeed

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

                lastColState :: tail ->
                    generateRowStepInternal
                        False
                        { rowState
                            | row = Debug.log "Backup up to row" <| row - 1
                            , col = lastColState.col
                            , board = lastColState.board
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

    else if newCol && col >= (board.cols - 1) then
        ( { state | col = board.cols }
        , seed
        )

    else
        let
            ( ( realCol, realPossibilities, newStack ), seed2 ) =
                if newCol then
                    let
                        ( choices, sd2 ) =
                            cellChoices row (col + 1) board seed

                        just0 =
                            choices == [ 0 ]
                    in
                    ( ( col + 1
                      , choices
                      , if col < 0 then
                            []

                        else
                            ( board, possibilities, just0 ) :: colStack
                      )
                    , sd2
                    )

                else
                    ( ( col, possibilities, colStack ), seed )
        in
        let
            ( maybeVal, newPossibilities, newSeed ) =
                case realPossibilities of
                    [] ->
                        ( Nothing, [], seed )

                    [ onePossibility ] ->
                        ( Just onePossibility, [], seed )

                    _ ->
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
                case newStack of
                    [] ->
                        -- we've tried everything for this column. Fail.
                        ( { state
                            | success = False
                            , board = Board.set row realCol 0 board
                            , possibilities = []
                            , colStack = []
                          }
                        , seed
                        )

                    ( lastBoard, lastPossibilities, only0 ) :: tail ->
                        if only0 then
                            ( { state
                                | success = False
                                , col = 0
                                , board = Board.set row realCol 0 board
                                , possibilities = []
                                , colStack = []
                              }
                            , seed
                            )

                        else
                            generateColStep
                                False
                                { state
                                    | col = realCol - 1
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
