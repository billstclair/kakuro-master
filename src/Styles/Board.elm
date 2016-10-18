----------------------------------------------------------------------
--
-- Styles/Board.elm
-- The CSS Stylesheet for the Kakuro page
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Styles.Board exposing
  ( style, BClass(..), class, classes )

import Css exposing (Sel(..))
import Html.Attributes

type BClass = Table
            | CellTd
            | CellButton
            | RegularCellButton
            | KeypadTd
            | KeypadButton
            | LabelButton
            | EmptyCellBackground
            | UnfilledCellButton
            | LabelTable
            | LabelTr
            | LabelTd
            | BottomLabelTd
            | RightLabelTd
            | Hint
            | Error
            | ErrorButton
            | SelectedError
            | SelectedErrorButton
            | Selected

imports : List String
imports = []

rule: a -> b -> { selectors : a, descriptor : b }
rule selectors descriptor =
  { selectors = selectors
  , descriptor = descriptor
  }

rules =
  [ rule
      [ Type "tr" ]
      [ ("padding", "0")
      , ("margin", "0")
      , ("board", "none")
      ]
  , rule
      [ Type "td" ]
      [ ("width", "1.5em")
      , ("height", "1.5em")
      , ("border", "1px solid black")
      , ("padding", "2px")
      , ("margin", "0")
      , ("font-size", "24pt")
      ]
  , rule
      [ Class Table ]
      [ ("font-family", "\"Lucida Console\", Monaco, monospace")
      , ("border", "1px solid black")
      ]
  , rule
      [ Class CellTd ]
      [ ("text-align", "center")
      ]
  , rule
      [ Class CellButton ]
      [ ("width", "99%")
      , ("height", "99%")
      , ("border", "none")
      ]
  , rule
      [ Class RegularCellButton ]
      [ ("background-color", "#ffffff")
      ]
  , rule
      [ Class KeypadTd ]
      [ ("text-align", "center")
      , ("width", "2.0em")
      , ("height", "2.0em")
      , ("color", "#ffffff")
      , ("background-color", "#808080")
      ]
  , rule
      [ Type "button" ]
      [ ("outline", "none")
      , ("font-size", "24pt")
      ]
  , rule
      [ Type "button::-moz-focus-inner" ]
      [ ("padding", "0")
      , ("border", "none")
      , ("margin-top", "-2px")
      , ("margin-bottom", "-2px")
      ]
  , rule
      [ Class KeypadButton ]
      [ ("background-color", "#808080")
      , ("color", "#ffffff")
      , ("width", "99%")
      , ("height", "99%")
      , ("margin", "0")
      , ("padding", "0")
      ]
  , rule
      [ Class LabelButton ]
      [ ("width", "99%")
      , ("height", "99%")
      , ("background-image", "url('images/diagonal-line-200x200.png')")
      , ("border", "none")
      , ("margin", "0")
      , ("padding", "0")
      ]
  , rule
      [ Class EmptyCellBackground ]
      [ ("background-color", "#808080")
      , ("padding", "2px")
      ]
  , rule
      [ Class UnfilledCellButton ]
      [ ("background-color", "#ffffff")
      ]
  , rule
      [ Class LabelTable]
      [ ("width", "99%")
      , ("height", "99%")
      , ("border", "none")
      , ("padding", "0")
      , ("margin", "0")
      ]
  , rule
      [ Class LabelTr]
      [("width", "99%")
      , ("height", "99%")
      , ("padding", "0")
      , ("margin", "0")
      ]
  , rule
      [ Class LabelTd]
      [ ("font-size", "10pt")
      , ("border", "none")
      , ("padding", "0")
      , ("margin", "0")
      ]
  , rule
      [ Class BottomLabelTd]
      [ ("font-weight", "bold")
      , ("vertical-align", "center")
      , ("text-align", "right")
      , ("font-size", "10pt")
      , ("border", "none")
      ]
  , rule
      [ Class RightLabelTd]
      [ ("font-weight", "bold")
      , ("vertical-align", "bottom")
      , ("text-align", "center")
      , ("font-size", "10pt")
      , ("border", "none")
      ]
  , rule
      [ Class Hint]
      [ ("width", "100%")
      , ("height", "100%")
      , ("font-size", "12pt")
      , ("font-weight", "bold")
      ]
  , rule
      [ Class Error]
      []
  , rule
      [ Class ErrorButton]
      [ ("background-color", "red")
      ]
  , rule
      [ Class SelectedError]
      [ ("border", "3px solid black")
      , ("padding", "0")
      ]
  , rule
      [ Class SelectedErrorButton]
      [ ("background-color", "#FF8080")
      , ("color", "white")
      ]
  , rule
      [ Class Selected]
      [ ("border", "3px solid black")
      , ("padding", "0")
      ]
  ]

stylesheet = Css.stylesheet imports rules

-- This is for inclusion at the beginning of the Board div
style = Css.style [ Html.Attributes.scoped True ] stylesheet

-- For use in the attributes of Html elements
class = stylesheet.class
classes = stylesheet.classes
