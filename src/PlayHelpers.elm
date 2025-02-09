----------------------------------------------------------------------
--
-- PlayHelpers.elm
-- Functions to compute data for providing user feedback
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module PlayHelpers exposing (computeFilledCellClasses, isAllDone, possibilities)

import Board exposing (Board, get, set)
import List.Extra as LE
import SharedTypes exposing (BClassBoard, BClassMatrix, GameState, IntBoard)
import SimpleMatrix
    exposing
        ( IntMatrix
        , Location
        , Matrix
        , col
        , incColBy
        , incRowBy
        , loc
        , row
        )
import Styles.Board exposing (BClass(..), class, classes)


doneColLoop : Int -> Int -> Int -> IntBoard -> IntBoard -> Bool
doneColLoop rowidx colidx cols board guesses =
    if colidx >= cols then
        True

    else if Board.get rowidx colidx board /= Board.get rowidx colidx guesses then
        False

    else
        doneColLoop rowidx (colidx + 1) cols board guesses


doneRowLoop : Int -> Int -> IntBoard -> IntBoard -> Bool
doneRowLoop rowidx rows board guesses =
    if rowidx >= rows then
        True

    else if doneColLoop rowidx 0 board.cols board guesses then
        doneRowLoop (rowidx + 1) rows board guesses

    else
        False


markBadSumColSegment : Int -> Int -> Int -> List Int -> BClassBoard -> BClassBoard
markBadSumColSegment row col sum acc res =
    markBadSumSegment row col sum acc incRowBy SimpleMatrix.updateRowRangeWithValue res


markFilledColDuplicates : Int -> Int -> Int -> List Int -> BClassBoard -> BClassBoard
markFilledColDuplicates guess rowidx colidx acc res =
    markFilledDuplicates
        guess
        rowidx
        colidx
        (\i -> incRowBy (negate i))
        acc
        res



-- If row >= rowss, and all elements of acc are non-zero, add Just Error
-- to all of the corresponding elements of res.
-- Otherwise, if the guess at (row,col) is already in acc, add Just Error to res.
-- Finally, recurse with row+1.


computeFilledColClassesRows : Int -> Int -> Int -> IntBoard -> IntBoard -> Int -> List Int -> BClassBoard -> BClassBoard
computeFilledColClassesRows row col rows board guesses sum acc res =
    let
        val =
            Board.get row col board

        guess =
            Board.get row col guesses

        res2 =
            if sum > 0 && guess /= 0 then
                markFilledColDuplicates guess row col acc res

            else
                res

        res3 =
            if val == 0 && sum > 0 then
                markBadSumColSegment row col sum acc res2

            else
                res2

        sum2 =
            if val == 0 then
                0

            else
                sum + val

        acc2 =
            if val == 0 then
                []

            else
                guess :: acc
    in
    if row >= rows then
        res3

    else
        computeFilledColClassesRows
            (row + 1)
            col
            rows
            board
            guesses
            sum2
            acc2
            res3



-- Add to res the display class for each of the guesses.


computeFilledColClasses : Int -> IntBoard -> IntBoard -> BClassBoard -> BClassBoard
computeFilledColClasses col board guesses res =
    computeFilledColClassesRows 0 col board.rows board guesses 0 [] res


type alias BClassMatrixRangeUpdater =
    Location -> Int -> Maybe BClass -> BClassMatrix -> BClassMatrix


type alias LocationIncrementor =
    Int -> Location -> Location


markBadSumSegment : Int -> Int -> Int -> List Int -> LocationIncrementor -> BClassMatrixRangeUpdater -> BClassBoard -> BClassBoard
markBadSumSegment row col sum acc incrementor updater res =
    if List.member 0 acc then
        res

    else
        let
            accsum =
                List.foldr (+) 0 acc
        in
        if accsum == sum then
            res

        else
            let
                len =
                    List.length acc

                start =
                    incrementor (negate len) ( row, col )
            in
            { res
                | array =
                    updater start len (Just Error) res.array
            }



-- If acc contains no zeroes, and doesn't sum to res,
-- Mark (row,col) and List.len acc back from there as Errors


markBadSumRowSegment : Int -> Int -> Int -> List Int -> BClassBoard -> BClassBoard
markBadSumRowSegment row col sum acc res =
    markBadSumSegment
        row
        col
        sum
        acc
        incColBy
        SimpleMatrix.updateColRangeWithValue
        res


markFilledDuplicates : Int -> Int -> Int -> (Int -> Location -> Location) -> List Int -> BClassBoard -> BClassBoard
markFilledDuplicates guess row col incrementor acc res =
    if guess == 0 then
        --Shouldn't happen. Famous last words.
        res

    else
        let
            idx =
                LE.elemIndex guess acc
        in
        case idx of
            Nothing ->
                res

            Just i ->
                let
                    ( row_, col_ ) =
                        incrementor (i + 1) ( row, col )
                in
                Board.set row col (Just Error) res
                    |> Board.set row_ col_ (Just Error)



-- If guess is in acc, then mark both (row,col) and (row,col - (position guess acc))
-- as Errors


markFilledRowDuplicates : Int -> Int -> Int -> List Int -> BClassBoard -> BClassBoard
markFilledRowDuplicates guess rowidx colidx acc res =
    markFilledDuplicates
        guess
        rowidx
        colidx
        (\i -> incColBy (negate i))
        acc
        res



-- If row is zero, call computedFilledColClasses to add the
-- display classes to res for the given col.
-- If col >= cols, and all elements of acc are non-zero, add Just Error
-- to all of the corresponding elements of res.
-- Otherwise, if the guess at (row,col) is already in acc, add Just Error to res.
-- Finally, recurse with col+1.


computeFilledRowClassesCols : Int -> Int -> Int -> IntBoard -> IntBoard -> Int -> List Int -> BClassBoard -> BClassBoard
computeFilledRowClassesCols row col cols board guesses sum acc res =
    let
        res2 =
            if row == 0 && col < cols then
                computeFilledColClasses col board guesses res

            else
                res

        val =
            Board.get row col board

        guess =
            Board.get row col guesses

        res3 =
            if sum > 0 && guess /= 0 then
                markFilledRowDuplicates guess row col acc res2

            else
                res2

        res4 =
            if val == 0 && sum > 0 then
                markBadSumRowSegment row col sum acc res3

            else
                res3

        sum2 =
            if val == 0 then
                0

            else
                sum + val

        acc2 =
            if val == 0 then
                []

            else
                guess :: acc
    in
    if col >= cols then
        res4

    else
        computeFilledRowClassesCols
            row
            (col + 1)
            cols
            board
            guesses
            sum2
            acc2
            res4



-- Add to res the display class for each of the guesses in the given row.
-- Then recurse for the next row, until row >= rows.


computeFilledRowClassesLoop : Int -> Int -> IntBoard -> IntBoard -> BClassBoard -> BClassBoard
computeFilledRowClassesLoop row rows board guesses res =
    if row >= rows then
        res

    else
        let
            res2 =
                computeFilledRowClassesCols row 0 board.cols board guesses 0 [] res
        in
        computeFilledRowClassesLoop (row + 1) rows board guesses res2



-- Add to res the display class for each of the guesses.


computeFilledRowClasses : IntBoard -> IntBoard -> BClassBoard -> BClassBoard
computeFilledRowClasses board guesses res =
    computeFilledRowClassesLoop 0 board.rows board guesses res


possLoop : Int -> Int -> Int -> Int -> List Int -> List Int -> List (List Int) -> List (List Int)
possLoop start count sumSoFar sum this used res =
    --  let (a,b,c,d,e,f,g) = log "(start,count,sumSoFar,sum,this,used,res)" (start,count,sumSoFar,sum,this,used,res)
    --  in
    if count <= 0 || sumSoFar > sum then
        if sumSoFar == sum then
            List.reverse this :: res

        else
            res

    else if start > 9 then
        res

    else
        let
            res2 =
                if List.member start used then
                    res

                else
                    possLoop
                        (start + 1)
                        (count - 1)
                        (sumSoFar + start)
                        sum
                        (start :: this)
                        (start :: used)
                        res
        in
        possLoop (start + 1) count sumSoFar sum this used res2



--
-- Exported functions
--
-- Returns true if the two arguments are identical


isAllDone : IntBoard -> IntBoard -> Bool
isAllDone board guesses =
    doneRowLoop 0 board.rows board guesses



-- Returns a Board containing a Maybe BClass for rendering each cell of
-- the board. It will be Nothing for cells that have no guess.


computeFilledCellClasses : IntBoard -> IntBoard -> BClassBoard
computeFilledCellClasses board guesses =
    Board.make board.rows board.cols Nothing
        |> computeFilledRowClasses board guesses


possibilities : Int -> Int -> List Int -> List (List Int)
possibilities sum count used =
    List.reverse (possLoop 1 count 0 sum [] used [])
