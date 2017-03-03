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

module Styles.Board exposing (style, BClass(..), class, classes)

import Css exposing (Sel(..))
import Html.Attributes

type BClass
    = Table
    | Helper
    | HelperLine
    | Error
      -- SVG Classes
    | SvgLabel
    | SvgSlash
    | SvgLabelText
    | SvgCell
    | SvgClick
    | SvgSelected
    | SvgSelectedSmall
    | SvgCellColor
    | SvgDoneColor
    | SvgErrorColor
    | SvgSelectedErrorColor
    | SvgCellText
    | SvgHintText
    | SvgEmptyCell
    | SvgKeypad
    | SvgKeypadText
    | SvgKeypadColor
    | SvgKeypadHighlightColor
    | SvgKeypadExploratoryColor

imports : List String
imports =
    []

rule : a -> b -> { selectors : a, descriptor : b }
rule selectors descriptor =
    { selectors = selectors
    , descriptor = descriptor
    }

greenColor : String
greenColor =
    "#E0FFE0"

rules =
    [ rule
        [ Class Table ]
        [ -- ( "font-family", "\"Lucida Console\", Monaco, monospace" )
          ( "border", "1px solid black" )
        ]
    , rule
        [ Class Helper ]
        [ ( "font-size", "10pt" )
        --, ( "font-weight", "bold")
        ]
    , rule
        [ Class HelperLine ]
        [ ( "width", "100%" )
        , ( "height", "1.2em" )
        , ( "white-space", "nowrap" )
        , ( "overflow-x", "auto" )
        ]
    , rule
        [ Class Error ]
        [ ( "background-color", "red" )
        , ( "color", "white" )
        ]
      -- SVG classes
    , rule
        [ Class SvgLabel ]
        [ ( "padding", "2px" )
        , ( "fill", "#ececec" )
        , ( "stroke", "white" )
        , ( "stroke-width", "1" )
        ]
    , rule
        [ Class SvgSlash ]
        [ ( "stroke", "#808080" )
        , ( "stroke-width", "3" )
        , ( "stroke-linecap", "round" )
        ]
    , rule
        [ Class SvgLabelText ]
        [ ( "fill", "black" )
        , ( "font-weight", "bold" )
        ]
    , rule
        [ Class SvgCell ]
        [ ( "fill-opacity", "1.0" )
        , ( "stroke", "black" )
        , ( "stroke-width", "1px" )
        ]
    , rule
        [ Class SvgClick ]
        [ ( "fill-opacity", "0.0" )
        , ( "stroke-width", "0" )
        ]
    , rule
        [ Class SvgCellColor ]
        [ ( "fill", "white" )
        ]
    , rule
        [ Class SvgDoneColor ]
        [ ( "fill", greenColor )
        ]
    , rule
        [ Class SvgErrorColor ]
        [ ( "fill", "red" )
        ]
    , rule
        [ Class SvgSelectedErrorColor ]
        [ ( "fill", "#FF8080" )
        ]
    , rule
        [ Class SvgSelected ]
        [ ( "stroke-width", "4px" )
        ]
    , rule
        [ Class SvgSelectedSmall ]
        [ ( "stroke-width", "3px" )
        ]
    , rule
        [ Class SvgCellText ]
        [ ( "font-weight", "bold" )
        ]
    , rule
        [ Class SvgHintText ]
        [ ( "font-weight", "bold" )
        ]
    , rule
        [ Class SvgEmptyCell ]
        [ ( "fill", "#808080" )
        , ( "stroke", "black" )
        , ( "stroke-width", "1" )
        ]
    , rule
        [ Class SvgKeypad ]
        [ ( "fill", "#808080" )
        , ( "stroke", "white" )
        , ( "stroke-width", "1" )
        ]
    , rule
        [ Class SvgKeypadText ]
        [ -- ( "font-family", "\"Lucida Console\", Monaco, monospace" )
          ( "font-weight", "bold" )
        ]
    , rule
        [ Class SvgKeypadColor ]
        [ ( "fill", "#ffffff" ) ]
    , rule
        [ Class SvgKeypadHighlightColor ]
        [ ( "fill", "black" ) ]
    , rule
        [ Class SvgKeypadExploratoryColor ]
        [ ( "fill", "#a0b0ff" ) ]
    ]

stylesheet =
    Css.stylesheet imports rules

-- This is for inclusion at the beginning of the Board div

style =
    Css.style [ Html.Attributes.scoped True ] stylesheet

-- For use in the attributes of Html elements

class =
    stylesheet.class

classes =
    stylesheet.classes
