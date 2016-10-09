----------------------------------------------------------------------
--
-- KakuroStylesheet.elm
-- The CSS Stylesheet for the Kakuro page
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module KakuroStylesheet exposing
  ( style, KClass(..), KId(..), id, class
  )

import Css exposing (Sel(..))
import Html.Attributes

type KClass = BoardCellClass
            | BoardLabelClass
            | ControlsClass

type KId = BoardId
         | TopInputId
         | FooterId

imports : List String
imports = []

rule: a -> b -> { selectors : a, descriptor : b }
rule selectors descriptor =
  { selectors = selectors
  , descriptor = descriptor
  }

rules =
  [ rule
      [ Id BoardId ]
      [ ("font-family", "\"Lucida Console\", Monaco, monospace")
      , ("font-size", "18pt")
      , ("padding", "2px")
      , ("border", "1px solid black")
      ]
  , rule
      [ Id TopInputId ]
      [ ("margin-bottom", "0.5em")
      ]
  , rule
      [ Id FooterId ]
      [ ("margin-top", "2em")
      ]
  , rule
      [ Class ControlsClass ]
      [ ("font-size", "14pt")
      ]
  , rule
      [ Class BoardCellClass ]
      [ ("padding", "4px")
      , ("textAlign", "center")
      , ("border", "1px solid black")
      ]
  , rule
      [ Class BoardLabelClass ]
      [ ("padding", "4px")
      , ("font-size", "50%")
      , ("text-align", "center")
      , ("border", "1px solid black")
      ]
  ]

stylesheet = Css.stylesheet imports rules

-- This is for inclusion at the beginning of the outermost div
style = Css.style [ Html.Attributes.scoped True ] stylesheet

-- For use in the attributes of Html elements
-- E.g. id Board
id = stylesheet.id
class = stylesheet.class
