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
            | KeypadTd
            | KeypadDiv
            | KeypadButton
            | KeypadButtonHighlight
            | Label
            | Helper
            | EmptyCell
            | EmptyCellBackground
            | UnfilledCell
            | LabelTable
            | LabelTr
            | LabelTd
            | BottomLabelTd
            | RightLabelTd
            | Hint
            | Error
            | SelectedError
            | Selected
            | Done
            -- SVG Classes
            | SvgLabel
            | SvgSlash
            | SvgLabelText
            | SvgCell
            | SvgClick
            | SvgSelected
            | SvgCellColor
            | SvgDoneColor
            | SvgErrorColor
            | SvgSelectedErrorColor
            | SvgCellText
            | SvgHintText
            | SvgEmptyCell

imports : List String
imports = []

rule: a -> b -> { selectors : a, descriptor : b }
rule selectors descriptor =
  { selectors = selectors
  , descriptor = descriptor
  }

greenColor : String
greenColor = "#E0FFE0"

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
      [ Class KeypadTd ]
      [ ("text-align", "center")
      , ("width", "2.0em")
      , ("height", "2.0em")
      , ("color", "#ffffff")
      , ("background-color", "#808080")
      ]
  , rule
      [ Class KeypadDiv ]
      [ ("font-size", "30pt")
      ]
  ,rule
      [ Type "button" ]
      [ ("outline", "none") ]
  , rule
      [ Class KeypadButton ]
      [ ("font-size", "30pt")
      , ("background-color", "#808080")
      , ("color", "#ffffff")
      , ("width", "99%")
      , ("height", "99%")
      , ("margin", "0")
      , ("padding", "0")
      ]
  , rule
    [ Class KeypadButtonHighlight ]
    [ ("color", "black") ]
  , rule
      [ Class EmptyCell ]
      [ ("width", "99%")
      , ("height", "99%")
      , ("background-color", "#808080")
      ]
  , rule
      [ Class EmptyCellBackground ]
      [ ("background-color", "#808080")
      ]
  , rule
      [ Class UnfilledCell ]
      [ ("width", "99%")
      , ("height", "99%")
      , ("background-color", "#ffffff")
      ]
  , rule
      [ Class LabelTable]
      [ ("width", "99%")
      , ("height", "99%")
      , ("border", "none")
      ]
  , rule
      [ Class LabelTr]
      [("width", "50%")
      , ("height", "99%")
      ]
  , rule
      [ Class LabelTd]
      [ ("width", "50%")
      , ("font-size", "10pt")
      , ("border", "none")
      , ("padding", "0")
      , ("margin", "0")
      ]
  , rule
      [ Class Helper ]
      [ ("font-size", "10pt")
      ]
  , rule
      [ Class Label ]
      [ ("width", "99%")
      , ("height", "99%")
      , ("background-image", "url('images/diagonal-line-200x200.png')")
      ]
  , rule
      [ Class BottomLabelTd]
      [ ("font-weight", "bold")
      , ("vertical-align", "center")
      , ("text-align", "right")
      , ("width"," 50%")
      , ("font-size", "10pt")
      , ("border", "none")
      ]
  , rule
      [ Class RightLabelTd]
      [ ("font-weight", "bold")
      , ("vertical-align", "bottom")
      , ("text-align", "center")
      , ("width", "50%")
      , ("font-size", "10pt")
      , ("border", "none")
      ]
  , rule
      [ Class Hint]
      [ --("width", "100%")
      --, ("height", "100%")
       ("font-size", "10pt")
      , ("font-weight", "bold")
      ]
  , rule
      [ Class Error]
      [ ("background-color", "red")
      , ("color", "white")
      ]
  , rule
      [ Class SelectedError]
      [ ("background-color", "#FF8080")
      , ("color", "white")
      , ("border", "3px solid black")
      , ("padding", "0")
      ]
  , rule
      [ Class Selected]
      [ ("border", "3px solid black")
      , ("padding", "0")
      ]
  , rule
      [ Class Done]
      [ ( "background-color", greenColor )
      ]

  -- SVG classes
  , rule
     [ Class SvgLabel ]
     [ ("padding", "2px")
     , ("fill", "#f0f0f0")
     , ("stroke", "white")
     , ("stroke-width", "1")
     ]
  , rule
     [ Class SvgSlash ]
     [ ("stroke", "#808080")
     , ("stroke-width", "3")
     ]
  , rule
     [ Class SvgLabelText ]
     [ ("fill", "black")
     , ("font-weight", "bold")
     ]
  , rule
     [ Class SvgCell ]
     [ ("fill-opacity", "0.0")
     , ("stroke", "black")
     , ("stroke-width", "1px")
     ]
  , rule
      [ Class SvgClick ]
      [ ("fill-opacity", "0.0")
      , ("stroke-width", "0")
      ]
  , rule
      [ Class SvgCellColor ]
      [ ("fill", "white")
      ]
  , rule
     [ Class SvgDoneColor ]
     [ ("fill", greenColor)
     ]
  , rule
     [ Class SvgErrorColor ]
     [ ("fill", "red")
     ]
  , rule
     [ Class SvgSelectedErrorColor ]
     [ ("fill", "#FF8080")
     ]
  , rule
     [ Class SvgSelected ]
     [ ("stroke-width", "3px")
     ]
  , rule
     [ Class SvgCellText ]
     [ ("font-weight", "bold")
     ]
  , rule
     [ Class SvgHintText ]
     [ ("font-weight", "bold")
     ]
  , rule
     [ Class SvgEmptyCell ]
     [ ("fill", "#808080")
     , ("stroke", "black")
     , ("stroke-width", "1")
     ]
  ]

stylesheet = Css.stylesheet imports rules

-- This is for inclusion at the beginning of the Board div
style = Css.style [ Html.Attributes.scoped True ] stylesheet

-- For use in the attributes of Html elements
class = stylesheet.class
classes = stylesheet.classes
