----------------------------------------------------------------------
--
-- SimpleMatrix.elm
-- A lighter-weight matrix package than chendrix/elm-matrix.
-- I mostly did it for updateRange, which is highly cool.
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module SimpleMatrix exposing
    ( IntMatrix
    , Location
    , Matrix
    , col
    , cols
    , elemIndex
    , fill
    , find
    , get
    , incCol
    , incColBy
    , incRow
    , incRowBy
    , initialize
    , loc
    , row
    , rows
    , set
    , updateColRange
    , updateColRangeUntil
    , updateColRangeWithValue
    , updateColRangeWithValueUntil
    , updateRange
    , updateRangeUntil
    , updateRowRange
    , updateRowRangeUntil
    , updateRowRangeWithValue
    , updateRowRangeWithValueUntil
    )

import Array exposing (Array)



{-
   --
   -- Example
   --

   m: IntMatrix
   m = fill 3 10 0

   m' = updateColRange (1, 2) 5 (\loc m -> col loc) m
   m'' = updateColRangeUntil (1, 2) (\loc -> (col loc) == 7) (\loc m -> col loc) m

-}


type alias Location =
    ( Int, Int )


loc : Int -> Int -> Location
loc rowa cola =
    ( rowa, cola )


row : Location -> Int
row loca =
    Tuple.first loca


col : Location -> Int
col loca =
    Tuple.second loca


incRowBy : Int -> Location -> Location
incRowBy increment loca =
    ( row loca + increment, col loca )


incColBy : Int -> Location -> Location
incColBy increment loca =
    ( row loca, col loca + increment )


incRow : Location -> Location
incRow loca =
    incRowBy 1 loca


incCol : Location -> Location
incCol loca =
    incColBy 1 loca


type alias Matrix a =
    Array (Array a)


type alias IntMatrix =
    Matrix Int


fill : Int -> Int -> a -> Matrix a
fill rowsa colsa value =
    Array.repeat colsa value
        |> Array.repeat rowsa


initializeCols : Int -> Int -> Int -> (Location -> a) -> Array a -> Array a
initializeCols rowidx colidx colsa initializer rowa =
    if colidx >= colsa then
        rowa

    else
        let
            value =
                initializer ( rowidx, colidx )
        in
        initializeCols
            rowidx
            (colidx + 1)
            colsa
            initializer
            (Array.set colidx value rowa)


initializeRows : Int -> Int -> Int -> (Location -> a) -> Matrix a -> Matrix a
initializeRows rowidx rowsa colsa initializer res =
    if rowidx >= rowsa then
        res

    else
        let
            rowa =
                Array.get rowidx res

            res_ =
                case rowa of
                    Nothing ->
                        res

                    Just r ->
                        let
                            row_ =
                                initializeCols rowidx 0 colsa initializer r
                        in
                        Array.set rowidx row_ res
        in
        initializeRows
            (rowidx + 1)
            rowsa
            colsa
            initializer
            res_


initialize : Int -> Int -> (Location -> a) -> Matrix a
initialize rowsa colsa initializer =
    let
        default =
            initializer ( 0, 0 )

        res =
            fill rowsa colsa default
    in
    initializeRows 0 rowsa colsa initializer res


rows : Matrix a -> Int
rows matrix =
    Array.length matrix


cols : Matrix a -> Int
cols matrix =
    let
        rowa =
            Array.get 0 matrix
    in
    case rowa of
        Nothing ->
            0

        Just rowb ->
            Array.length rowb


get : Location -> Matrix a -> Maybe a
get loca matrix =
    let
        ( rowidx, colidx ) =
            loca
    in
    case Array.get rowidx matrix of
        Nothing ->
            Nothing

        Just r ->
            case Array.get colidx r of
                Nothing ->
                    Nothing

                something ->
                    something


set : Location -> a -> Matrix a -> Matrix a
set loca val matrix =
    let
        ( rowidx, colidx ) =
            loca
    in
    case Array.get rowidx matrix of
        Nothing ->
            matrix

        Just r ->
            Array.set rowidx (Array.set colidx val r) matrix


indexColLoop : Int -> Int -> Int -> (a -> Bool) -> Array a -> Maybe ( Location, a )
indexColLoop rowidx colidx colsa pred rowa =
    if colidx >= colsa then
        Nothing

    else
        case Array.get colidx rowa of
            Nothing ->
                Nothing

            Just e ->
                if pred e then
                    Just ( ( rowidx, colidx ), e )

                else
                    indexColLoop rowidx (colidx + 1) colsa pred rowa


indexRowLoop : Int -> Int -> (a -> Bool) -> Matrix a -> Maybe ( Location, a )
indexRowLoop rowidx rowsa pred matrix =
    if rowidx >= rowsa then
        Nothing

    else
        case Array.get rowidx matrix of
            Nothing ->
                Nothing

            Just r ->
                case indexColLoop rowidx 0 (cols matrix) pred r of
                    Nothing ->
                        indexRowLoop (rowidx + 1) rowsa pred matrix

                    something ->
                        something


elemIndex : a -> Matrix a -> Maybe Location
elemIndex elem matrix =
    case indexRowLoop 0 (rows matrix) (\x -> x == elem) matrix of
        Nothing ->
            Nothing

        Just pair ->
            Just (Tuple.first pair)


find : (a -> Bool) -> Matrix a -> Maybe a
find pred matrix =
    case indexRowLoop 0 (rows matrix) pred matrix of
        Nothing ->
            Nothing

        Just pair ->
            Just (Tuple.second pair)


updateRange : a -> (a -> a) -> Int -> (a -> b -> b) -> b -> b
updateRange start incrementor count updater thing =
    if count <= 0 then
        thing

    else
        updateRange
            (incrementor start)
            incrementor
            (count - 1)
            updater
            (updater start thing)


updateRangeUntil : a -> (a -> a) -> (a -> Bool) -> (a -> b -> b) -> b -> b
updateRangeUntil start incrementor predicate updater thing =
    if predicate start then
        thing

    else
        updateRangeUntil
            (incrementor start)
            incrementor
            predicate
            updater
            (updater start thing)


updateRowRange : Location -> Int -> (Location -> Matrix a -> Matrix a) -> Matrix a -> Matrix a
updateRowRange loca count updater matrix =
    updateRange loca incRow count updater matrix


updateColRange : Location -> Int -> (Location -> Matrix a -> Matrix a) -> Matrix a -> Matrix a
updateColRange loca count updater matrix =
    updateRange loca incCol count updater matrix


updateRowRangeUntil : Location -> (Location -> Bool) -> (Location -> Matrix a -> Matrix a) -> Matrix a -> Matrix a
updateRowRangeUntil loca predicate updater matrix =
    updateRangeUntil loca incRow predicate updater matrix


updateColRangeUntil : Location -> (Location -> Bool) -> (Location -> Matrix a -> Matrix a) -> Matrix a -> Matrix a
updateColRangeUntil loca predicate updater matrix =
    updateRangeUntil loca incCol predicate updater matrix


valueUpdater : a -> Location -> Matrix a -> Matrix a
valueUpdater value loca matrix =
    let
        rowidx =
            row loca

        r =
            Array.get rowidx matrix
    in
    case r of
        Nothing ->
            matrix

        Just r_ ->
            Array.set rowidx (Array.set (col loca) value r_) matrix


updateRowRangeWithValue : Location -> Int -> a -> Matrix a -> Matrix a
updateRowRangeWithValue loca count value matrix =
    updateRowRange loca count (valueUpdater value) matrix


updateColRangeWithValue : Location -> Int -> a -> Matrix a -> Matrix a
updateColRangeWithValue loca count value matrix =
    updateColRange loca count (valueUpdater value) matrix


updateRowRangeWithValueUntil : Location -> (Location -> Bool) -> a -> Matrix a -> Matrix a
updateRowRangeWithValueUntil loca predicate value matrix =
    updateRowRangeUntil loca predicate (valueUpdater value) matrix


updateColRangeWithValueUntil : Location -> (Location -> Bool) -> a -> Matrix a -> Matrix a
updateColRangeWithValueUntil loca predicate value matrix =
    updateColRangeUntil loca predicate (valueUpdater value) matrix
