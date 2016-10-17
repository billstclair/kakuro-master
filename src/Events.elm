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

module Events exposing (onClickWithId)

import Html exposing (Attribute)
import Html.Attributes exposing (id)
import Html.Events exposing (on)
import Json.Decode as Json

targetId : Json.Decoder String
targetId =
  Json.at ["target", "id"] Json.string

onClickWithId : (String -> msg) -> Attribute msg
onClickWithId msg =
  on "click" (Json.map msg targetId)
