---------------------------------------------------------------------
--
-- EncodeDecode.elm
-- Functions to encode state to JSON strings and decode it back into Elm
-- Copyright (c) 2016-2025 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module EncodeDecode exposing
    ( decodeGameState
    , decodeIapStates
    , decodeSavedModel
    , encodeGameState
    , encodeIapStates
    , encodeSavedModel
    , gameStateDecoder
    , iapStatesDecoder
    , savedModelDecoder
    )

import Array
import Board exposing (Board)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, field)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import SharedTypes
    exposing
        ( ExploreState
        , Flags
        , GameState
        , GameStateTimes
        , Hints
        , HintsBoard
        , IapProduct
        , IapPurchase
        , IapState
        , IntBoard
        , Labels
        , LabelsBoard
        , Page(..)
        , SavedModel
        , Selection
        )
import VersionedJson
    exposing
        ( ConverterDict
        , decodeVersionedJson
        , encodeVersionedJson
        )



--
-- Encoders
--


encodeXBoard : (x -> Value) -> Board x -> Value
encodeXBoard encodeX board =
    JE.object
        [ ( "rows", JE.int board.rows )
        , ( "cols", JE.int board.cols )
        , ( "defaultValue", encodeX board.defaultValue )
        , ( "spec"
          , case board.spec of
                Nothing ->
                    JE.null

                Just spec ->
                    JE.string spec
          )
        , ( "index"
          , case board.index of
                Nothing ->
                    JE.null

                Just index ->
                    JE.int index
          )
        , ( "array"
          , JE.array (\x -> JE.array encodeX x) board.array
          )
        ]


encodeIntBoard : IntBoard -> Value
encodeIntBoard board =
    encodeXBoard JE.int board


encodeLabels : Labels -> Value
encodeLabels labels =
    let
        ( x, y ) =
            labels
    in
    JE.list JE.int [ x, y ]


encodeLabelsBoard : LabelsBoard -> Value
encodeLabelsBoard board =
    encodeXBoard encodeLabels board


encodeHints : Hints -> Value
encodeHints hints =
    JE.list JE.int hints


encodeHintsBoard : HintsBoard -> Value
encodeHintsBoard board =
    encodeXBoard encodeHints board


encodeFlags : Flags -> Value
encodeFlags flags =
    JE.object
        [ ( "isHintInput", JE.bool flags.isHintInput )
        , ( "showPossibilities", JE.bool flags.showPossibilities )
        , ( "firstGuess", JE.int flags.firstGuess )
        , ( "keyClickSound", JE.bool flags.keyClickSound )
        ]


encodeSelection : Maybe Selection -> Value
encodeSelection maybeSelection =
    case maybeSelection of
        Nothing ->
            JE.null

        Just ( x, y ) ->
            JE.list JE.int [ x, y ]


encodeExploreState : Maybe ExploreState -> Value
encodeExploreState maybeExploreState =
    case maybeExploreState of
        Nothing ->
            JE.null

        Just state ->
            JE.object
                [ ( "savedBoard", encodeIntBoard state.savedBoard )
                , ( "savedHints", encodeHintsBoard state.savedHints )
                , ( "guesses", encodeIntBoard state.guesses )
                , ( "firstGuess", JE.int state.firstGuess )
                , ( "firstGuessSelection", encodeSelection state.firstGuessSelection )
                ]


encodePair : ( Int, Int ) -> Value
encodePair pair =
    let
        ( x, y ) =
            pair
    in
    JE.list JE.int [ x, y ]


encodeIndices : List ( Int, Int ) -> Value
encodeIndices indices =
    JE.list encodePair indices


encodePage : Page -> Value
encodePage page =
    case page of
        MainPage ->
            JE.string "main"

        HelpPage ->
            JE.string "help"

        TacticsPage ->
            JE.string "tactics"

        CreditsPage ->
            JE.string "credits"

        IapPage ->
            JE.string "purchases"

        AdvertisePage ->
            JE.string "advertise"



--
-- Decoders
--


{-| For compatibity with billstclair/elm-versioned-string,
which this code does not use, but tries to support old
-}
versionedJsonDecoder : JD.Decoder ( Int, String )
versionedJsonDecoder =
    JD.map2 (\a b -> ( a, b ))
        (field "version" JD.int)
        (field "value" JD.string)


maybeVersionedDecoder : Decoder a -> Decoder a
maybeVersionedDecoder userDecoder =
    JD.oneOf
        [ userDecoder
        , versionedJsonDecoder
            |> JD.andThen
                (\( _, s ) ->
                    case JD.decodeString userDecoder s of
                        Ok a ->
                            JD.succeed a

                        Err err ->
                            JD.fail <| JD.errorToString err
                )
        ]


xBoardDecoder : Decoder x -> Decoder (Board x)
xBoardDecoder xDecoder =
    let
        xad =
            JD.array (JD.array xDecoder)
    in
    JD.map6
        Board.makeWithAll
        (field "rows" JD.int)
        (field "cols" JD.int)
        (field "defaultValue" xDecoder)
        (field "spec" (JD.nullable JD.string))
        (field "index" (JD.nullable JD.int))
        (field "array" xad)


intBoardDecoder : Decoder IntBoard
intBoardDecoder =
    xBoardDecoder JD.int


listToLabels : List Int -> Labels
listToLabels list =
    case list of
        [ x, y ] ->
            ( x, y )

        _ ->
            ( 0, 0 )


labelsDecoder : Decoder Labels
labelsDecoder =
    JD.list JD.int
        |> JD.map listToLabels


labelsBoardDecoder : Decoder LabelsBoard
labelsBoardDecoder =
    xBoardDecoder labelsDecoder


hintsDecoder : Decoder Hints
hintsDecoder =
    JD.list JD.int


hintsBoardDecoder : Decoder HintsBoard
hintsBoardDecoder =
    xBoardDecoder hintsDecoder


type alias Flags1 =
    { isHintInput : Bool
    , showPossibilities : Bool
    }


flags1Decoder : Decoder Flags1
flags1Decoder =
    JD.map2
        Flags1
        (field "isHintInput" JD.bool)
        (field "showPossibilities" JD.bool)


flagsDecoder : Decoder Flags
flagsDecoder =
    JD.map4
        Flags
        (field "isHintInput" JD.bool)
        (field "showPossibilities" JD.bool)
        (field "firstGuess" JD.int)
        (JD.oneOf
            [ field "keyClickSound" JD.bool
            , JD.succeed True
            ]
        )


listToMaybeSelection : Maybe (List Int) -> Maybe Selection
listToMaybeSelection list =
    case list of
        Just [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


maybeSelectionDecoder : Decoder (Maybe Selection)
maybeSelectionDecoder =
    JD.nullable (JD.list JD.int)
        |> JD.map listToMaybeSelection


pairDecoder : Decoder ( Int, Int )
pairDecoder =
    JD.list JD.int
        |> JD.andThen twoListToPairDecoder


twoListToPairDecoder : List Int -> Decoder ( Int, Int )
twoListToPairDecoder list =
    case list of
        [ a, b ] ->
            JD.map2 (\aa bb -> ( aa, bb )) (JD.succeed a) (JD.succeed b)

        _ ->
            JD.fail <|
                "Malformed integer pair: "
                    ++ (List.map String.fromInt list
                            |> List.intersperse ", "
                            |> String.concat
                       )


indicesDecoder : Decoder (List ( Int, Int ))
indicesDecoder =
    JD.list pairDecoder


pageDecoder : Decoder Page
pageDecoder =
    JD.string |> JD.andThen pageHelp


pageHelp : String -> Decoder Page
pageHelp page =
    case page of
        "main" ->
            JD.succeed MainPage

        "help" ->
            JD.succeed HelpPage

        "tactics" ->
            JD.succeed TacticsPage

        "credits" ->
            JD.succeed CreditsPage

        "purchases" ->
            JD.succeed IapPage

        "advertise" ->
            JD.succeed AdvertisePage

        _ ->
            JD.fail <| "Bad page name: " ++ page


maybeExploreStateDecoder : Decoder (Maybe ExploreState)
maybeExploreStateDecoder =
    JD.nullable exploreStateDecoder


exploreStateDecoder : Decoder ExploreState
exploreStateDecoder =
    JD.map5
        ExploreState
        (field "savedBoard" intBoardDecoder)
        (field "savedHints" hintsBoardDecoder)
        (field "guesses" intBoardDecoder)
        (field "firstGuess" JD.int)
        (field "firstGuessSelection" maybeSelectionDecoder)



--
-- Current version encoder and decoder
--


encodeGameStateTimes : GameStateTimes -> Value
encodeGameStateTimes times =
    JE.object
        [ ( "timestamp", JE.int times.timestamp )
        , ( "elapsed", JE.int times.elapsed )
        ]


encodeGameState : GameState -> Value
encodeGameState gameState =
    JE.object
        [ ( "board", encodeIntBoard gameState.board )
        , ( "labels", encodeLabelsBoard gameState.labels )
        , ( "guesses", encodeIntBoard gameState.guesses )
        , ( "hints", encodeHintsBoard gameState.hints )
        , ( "flags", encodeFlags gameState.flags )
        , ( "selection", encodeSelection gameState.selection )
        , ( "exploreState", encodeExploreState gameState.exploreState )
        , ( "times", encodeGameStateTimes gameState.times )
        ]


encodeSavedModel : SavedModel -> Value
encodeSavedModel model =
    JE.object
        [ ( "kind", JE.int model.kind )
        , ( "index", JE.int model.index )
        , ( "indices", encodeIndices model.indices )
        , ( "gencount", JE.int model.gencount )
        , ( "gameState", encodeGameState model.gameState )
        , ( "page", encodePage model.page )
        , ( "timestamp", JE.int model.timestamp )
        ]


type alias GameStateOld =
    { timestamp : Float
    , elapsed : Float
    }


gameStateTimesDecoderOld : Decoder GameStateTimes
gameStateTimesDecoderOld =
    (JD.succeed GameStateOld
        |> required "timestamp" JD.float
        |> required "elapsed" JD.float
    )
        |> JD.andThen
            (\{ timestamp, elapsed } ->
                JD.succeed
                    { timestamp = round timestamp
                    , elapsed = round elapsed
                    }
            )


gameStateTimesDecoder : Decoder GameStateTimes
gameStateTimesDecoder =
    JD.oneOf
        [ gameStateTimesDecoder2
        , maybeVersionedDecoder gameStateTimesDecoderOld
        ]


gameStateTimesDecoder2 : Decoder GameStateTimes
gameStateTimesDecoder2 =
    JD.succeed GameStateTimes
        |> required "timestamp" JD.int
        |> required "elapsed" JD.int


decodeGameState : String -> Result JD.Error GameState
decodeGameState json =
    JD.decodeString gameStateDecoder json


gameStateDecoder : Decoder GameState
gameStateDecoder =
    maybeVersionedDecoder gameStateDecoder2


gameStateDecoder2 : Decoder GameState
gameStateDecoder2 =
    JD.succeed GameState
        |> required "board" intBoardDecoder
        |> required "labels" labelsBoardDecoder
        |> required "guesses" intBoardDecoder
        |> required "hints" hintsBoardDecoder
        |> required "flags" flagsDecoder
        |> required "selection" maybeSelectionDecoder
        |> required "exploreState" maybeExploreStateDecoder
        |> required "times" gameStateTimesDecoder


decodeSavedModel : String -> Result JD.Error SavedModel
decodeSavedModel json =
    JD.decodeString savedModelDecoder json


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    maybeVersionedDecoder savedModelDecoder2


floatToIntDecoder : Decoder Int
floatToIntDecoder =
    JD.oneOf
        [ JD.int
        , JD.float |> JD.andThen (round >> JD.succeed)
        ]


savedModelDecoder2 : Decoder SavedModel
savedModelDecoder2 =
    JD.succeed SavedModel
        |> required "kind" JD.int
        |> required "index" JD.int
        |> required "indices" indicesDecoder
        |> required "gencount" JD.int
        |> required "gameState" gameStateDecoder
        |> required "page" pageDecoder
        |> required "timestamp" floatToIntDecoder



---
--- In-App Purchase state
---
-- Encoders


encodeIapProduct : IapProduct -> Value
encodeIapProduct product =
    JE.object
        [ ( "productId", JE.string product.productId )
        , ( "title", JE.string product.title )
        , ( "description", JE.string product.description )
        , ( "price", JE.string product.price )
        ]


encodeIapPurchase : IapPurchase -> Value
encodeIapPurchase purchase =
    JE.object
        [ ( "productId", JE.string purchase.productId )
        , ( "transactionId", JE.string purchase.transactionId )
        , ( "date", JE.int purchase.date )
        ]


encodeIapState : IapState -> Value
encodeIapState state =
    JE.object
        [ ( "product", encodeIapProduct state.product )
        , ( "purchase", encodeIapPurchase state.purchase )
        ]


encodeIapStates : List IapState -> Value
encodeIapStates states =
    JE.list encodeIapState states



-- decoders


iapProductDecoder : Decoder IapProduct
iapProductDecoder =
    JD.succeed IapProduct
        |> required "productId" JD.string
        |> required "title" JD.string
        |> required "description" JD.string
        |> required "price" JD.string


iapPurchaseDecoder : Decoder IapPurchase
iapPurchaseDecoder =
    JD.succeed IapPurchase
        |> required "productId" JD.string
        |> required "transactionId" JD.string
        |> required "date" JD.int


iapStateDecoder : Decoder IapState
iapStateDecoder =
    JD.succeed IapState
        |> required "product" iapProductDecoder
        |> required "purchase" iapPurchaseDecoder


decodeIapStates : String -> Result JD.Error (List IapState)
decodeIapStates json =
    JD.decodeString iapStatesDecoder json


iapStatesDecoder : Decoder (List IapState)
iapStatesDecoder =
    JD.list iapStateDecoder
