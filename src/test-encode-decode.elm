----------------------------------------------------------------------
--
-- test-encode-decode.elm
-- Top-level test code for EncodeDecode.elm
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

--
-- This will work with elm-reactor
-- Start it in the main directory, not "src"
--

import EncodeDecode exposing ( GameState1, SavedModel1
                             , encodeGameState, encodeSavedModel
                             , decodeGameState, decodeSavedModel)

import Html exposing (Html, div, text)

main =
    Html.beginnerProgram
        { model = 1
        , view = view
        , update = update
        }

update : msg -> model -> model
update msg model =
    model

br : Html msg
br =
    Html.br [][]

roundTripGameState : String -> String
roundTripGameState json =
    case decodeGameState json of
      Err err -> err
      Ok gameState ->
          encodeGameState gameState
              |> decodeGameState
              |> toString

roundTripSavedModel : String -> String
roundTripSavedModel json =
    case decodeSavedModel json of
      Err err -> err
      Ok gameState ->
          encodeSavedModel gameState
              |> decodeSavedModel
              |> toString

view : model -> Html msg
view model =
    div []
        [ text <| "hash: " ++ roundTripGameState hash
        , br
        , text <| "kakuroDojo: " ++ roundTripSavedModel kakuroDojo
        ]

hash : String
hash =
    "{\"version\":7,\"board\":{\"rows\":6,\"cols\":6,\"defaultValue\":0,\"spec\":\".38.31.13.12..139..798..31.92.92.21.\",\"index\":1,\"array\":[[0,3,8,0,3,1],[0,1,3,0,1,2],[0,0,1,3,9,0],[0,7,9,8,0,0],[3,1,0,9,2,0],[9,2,0,2,1,0]]},\"labels\":{\"rows\":7,\"cols\":7,\"defaultValue\":[0,0],\"spec\":null,\"index\":null,\"array\":[[[0,0],[0,0],[0,4],[0,21],[0,0],[0,13],[0,3]],[[0,0],[11,0],[0,0],[0,0],[4,0],[0,0],[0,0]],[[0,0],[4,0],[0,0],[0,0],[3,22],[0,0],[0,0]],[[0,0],[0,0],[13,10],[0,0],[0,0],[0,0],[0,0]],[[0,0],[24,12],[0,0],[0,0],[0,0],[0,3],[0,0]],[[4,0],[0,0],[0,0],[11,0],[0,0],[0,0],[0,0]],[[11,0],[0,0],[0,0],[3,0],[0,0],[0,0],[0,0]]]},\"allDone\":false,\"guesses\":{\"rows\":6,\"cols\":6,\"defaultValue\":0,\"spec\":null,\"index\":null,\"array\":[[0,3,8,0,3,1],[0,1,3,0,1,2],[0,0,1,3,9,0],[0,7,9,8,0,0],[3,1,0,9,2,0],[9,2,0,2,1,0]]},\"hints\":{\"rows\":6,\"cols\":6,\"defaultValue\":[],\"spec\":null,\"index\":null,\"array\":[[[],[],[],[],[],[]],[[],[],[],[],[],[]],[[],[],[],[],[],[]],[[],[],[],[],[],[]],[[],[],[],[],[],[]],[[],[],[],[],[],[]]]},\"flags\":{\"isHintInput\":false,\"showPossibilities\":true},\"selection\":[2,2]}"

kakuroDojo: String
kakuroDojo =
    "{\"version\":7,\"kind\":6,\"index\":3,\"gencount\":2,\"gameState\":{\"version\":7,\"board\":{\"rows\":6,\"cols\":6,\"defaultValue\":0,\"spec\":\".24.98.96185..72....84..14938.39.59.\",\"index\":3,\"array\":[[0,2,4,0,9,8],[0,9,6,1,8,5],[0,0,7,2,0,0],[0,0,8,4,0,0],[1,4,9,3,8,0],[3,9,0,5,9,0]]},\"labels\":{\"rows\":7,\"cols\":7,\"defaultValue\":[0,0],\"spec\":null,\"index\":null,\"array\":[[[0,0],[0,0],[0,11],[0,34],[0,0],[0,17],[0,13]],[[0,0],[6,0],[0,0],[0,0],[17,15],[0,0],[0,0]],[[0,0],[29,0],[0,0],[0,0],[0,0],[0,0],[0,0]],[[0,0],[0,0],[9,0],[0,0],[0,0],[0,0],[0,0]],[[0,0],[0,4],[12,13],[0,0],[0,0],[0,17],[0,0]],[[25,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]],[[12,0],[0,0],[0,0],[14,0],[0,0],[0,0],[0,0]]]},\"allDone\":false,\"guesses\":{\"rows\":6,\"cols\":6,\"defaultValue\":0,\"spec\":null,\"index\":null,\"array\":[[0,2,4,0,9,8],[0,9,6,1,8,5],[0,0,7,2,0,0],[0,0,8,4,0,0],[0,0,9,3,8,0],[0,0,0,5,9,0]]},\"hints\":{\"rows\":6,\"cols\":6,\"defaultValue\":[],\"spec\":null,\"index\":null,\"array\":[[[],[],[],[],[9,8],[]],[[],[],[],[],[9,8],[]],[[],[],[7,6,5],[4,3,2],[],[]],[[],[],[8,9],[4,3],[],[]],[[1,3],[],[9,8],[],[9,8],[]],[[1,3],[],[],[6,5],[9,8],[]]]},\"flags\":{\"isHintInput\":false,\"showPossibilities\":true},\"selection\":[4,3]},\"time\":0}"
