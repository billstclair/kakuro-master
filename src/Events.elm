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

module Events exposing (onClickWithId, onKeyPress)

import Html exposing (Attribute)
import Html.Attributes exposing (id)
import Html.Events exposing (on, onWithOptions, keyCode, Options)
import Json.Decode as Json exposing ((:=))

targetId : Json.Decoder String
targetId =
  Json.at ["target", "id"] Json.string

onClickWithId : (String -> msg) -> Attribute msg
onClickWithId msg =
  on "click" (Json.map msg targetId)

key : Json.Decoder Int
key =
  ("key" := Json.int)

keyIdentifier : Json.Decoder Int
keyIdentifier =
  ("keyIdentifier" := Json.int)

code : Json.Decoder Int
code =
  ("code" := Json.int)

preventDefaultOptions =
  Options False True

onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
  onWithOptions
    "keypress"
    preventDefaultOptions
    (Json.map tagger keyCode)
