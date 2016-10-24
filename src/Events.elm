----------------------------------------------------------------------
--
-- Events.elm
-- Render the game board.
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Events exposing ( onClickWithId, onClickWithString, onClickWithInt
                       , svgOnClickWithId )

import Html exposing (Attribute)
import Html.Attributes exposing (id)
import Html.Events exposing (on)
import Json.Decode as Json
import Svg.Events

targetId : Json.Decoder String
targetId =
  Json.at ["target", "id"] Json.string

onClickWithId : (String -> msg) -> Attribute msg
onClickWithId msg =
  on "click" (Json.map msg targetId)

svgOnClickWithId : (String -> msg) -> Attribute msg
svgOnClickWithId msg =
  Svg.Events.on "click" (Json.map msg targetId)

onClickWithString : (String -> msg) -> String -> Attribute msg
onClickWithString msg string =
  on "click" (Json.map msg <| Json.succeed string)

onClickWithInt : (Int -> msg) -> Int -> Attribute msg
onClickWithInt msg int =
  on "click" (Json.map msg <| Json.succeed int)
