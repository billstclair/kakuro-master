----------------------------------------------------------------------
--
-- Board.elm
-- Rectangular game board with integer elements
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Board exposing
    ( Board
    , make, makeWithInitial, makeWithSpec, makeWithSpecInitial, makeWithAll
    , get, set, getRow, setRow
    , kind, arrayFromNestedList, isBoardEmpty
    )

{-| Two-dimensional game board with integers as elements.

@docs Board
@docs make, makeWithInitial, makeWithSpec, makeWithSpecInitial, makeWithAll
@docs get, set, getRow, setRow
@docs kind, arrayFromNestedList, isBoardEmpty

-}

import Array exposing (Array)


{-| -}
type alias Board a =
    { rows : Int
    , cols : Int
    , defaultValue : a
    , spec : Maybe String
    , index : Maybe Int
    , array : Array (Array a)
    }


makeRow : Int -> a -> Array a
makeRow cols defaultValue =
    Array.repeat cols defaultValue


{-| Create a new Board of the given size, initialized with the defaultValue value.

    make rows cols defaultValue

-}
make : Int -> Int -> a -> Board a
make rows cols defaultValue =
    makeWithInitial rows cols defaultValue defaultValue


{-| Create a new Board of the given size, initialized with a different value.

    makeWithInitial rows cols defaultValue initial

-}
makeWithInitial : Int -> Int -> a -> a -> Board a
makeWithInitial rows cols defaultValue initial =
    makeWithSpecInitial rows cols defaultValue initial Nothing


{-| Create a new Board of the given size, with an optional specification.

    makeWithSpec rows cols defaultValue spec

-}
makeWithSpec : Int -> Int -> a -> Maybe String -> Board a
makeWithSpec rows cols defaultValue spec =
    makeWithSpecInitial rows cols defaultValue defaultValue spec


{-| Create a new Board of the given size, with an optional specification,
and initialized with a different value than the default.

    makeWithSpec rows cols defaultValue initial spec

-}
makeWithSpecInitial : Int -> Int -> a -> a -> Maybe String -> Board a
makeWithSpecInitial rows cols defaultValue initial spec =
    let
        array =
            Array.repeat rows (makeRow cols initial)
    in
    makeWithAll rows cols defaultValue spec Nothing array


{-| Create a new board, specifying all properties.
Usually called by JSON decoders.

    makeWithAll rows cols defaultValue index spec array

-}
makeWithAll : Int -> Int -> a -> Maybe String -> Maybe Int -> Array (Array a) -> Board a
makeWithAll rows cols defaultValue spec index array =
    { rows = rows
    , cols = cols
    , defaultValue = defaultValue
    , index = index
    , spec = spec
    , array = array
    }


check : Board a -> Int -> Int -> Bool
check board row col =
    row >= 0 && row < board.rows && col >= 0 && col < board.cols


{-| Get a single element. Returns 0 if row or col is out of range.

    get row col board

-}
get : Int -> Int -> Board a -> a
get row col board =
    case Array.get row board.array of
        Nothing ->
            board.defaultValue

        Just r ->
            case Array.get col r of
                Nothing ->
                    board.defaultValue

                Just res ->
                    res


{-| Set a single element. Does nothing if row or col is out of range.

    set row col val board

-}
set : Int -> Int -> a -> Board a -> Board a
set row col val board =
    if not (check board row col) then
        board

    else
        case Array.get row board.array of
            Nothing ->
                board

            Just r ->
                { board
                    | array =
                        Array.set
                            row
                            (Array.set col val r)
                            board.array
                }


{-| Return the Array for the given row, or an all-zero array, if out of range.

    getRow row board

-}
getRow : Int -> Board a -> Array a
getRow row board =
    case Array.get row board.array of
        Nothing ->
            makeRow board.cols board.defaultValue

        Just r ->
            r


{-| Set row in board to rowArray. Do nothing if row is out of range.

    setRow row rowArray board

-}
setRow : Int -> Array a -> Board a -> Board a
setRow row rowArray board =
    { board | array = Array.set row rowArray board.array }


{-| Another name for .cols
-}
kind : Board a -> Int
kind =
    .cols


{-| Make a board array from a list.
-}
arrayFromNestedList : List (List a) -> Array (Array a)
arrayFromNestedList list =
    Array.fromList <| List.map Array.fromList list


areBoardColsEmpty : Int -> Int -> Array a -> a -> Bool
areBoardColsEmpty colIdx cols array value =
    if colIdx >= cols then
        True

    else
        case Array.get colIdx array of
            Nothing ->
                False

            Just v ->
                if v == value then
                    areBoardColsEmpty (colIdx + 1) cols array value

                else
                    False


areBoardRowsEmpty : Int -> Int -> Int -> Array (Array a) -> a -> Bool
areBoardRowsEmpty rowIdx rows cols array value =
    if rowIdx >= rows then
        True

    else
        case Array.get rowIdx array of
            Nothing ->
                False

            Just a ->
                if areBoardColsEmpty 0 cols a value then
                    areBoardRowsEmpty (rowIdx + 1) rows cols array value

                else
                    False


{-| True if every element of Board is its default value
-}
isBoardEmpty : Board a -> Bool
isBoardEmpty board =
    areBoardRowsEmpty 0 board.rows board.cols board.array board.defaultValue
