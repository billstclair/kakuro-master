----------------------------------------------------------------------
--
-- Styles/Page.elm
-- The CSS Stylesheet for the Kakuro page
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Styles.Page exposing (PClass(..), PId(..), class, id, style)

import Css exposing (Sel(..))
import Html.Attributes


type PClass
    = BoardCellClass
    | BoardLabelClass
    | ControlsClass
    | HelpTextClass
    | StarMenuButtonClass
    | FullWidthClass
    | ErrorClass
    | PrettyTable


type PId
    = BoardId
    | TopInputId
    | FooterId


imports : List String
imports =
    []


rule : a -> b -> { selectors : a, descriptor : b }
rule selectors descriptor =
    { selectors = selectors
    , descriptor = descriptor
    }


rules =
    [ rule
        [ Id BoardId ]
        [ ( "font-family", "'HelveticaNeue-Light', 'HelveticaNeue', Helvetica, Arial, sans-serif" )
        , ( "font-size", "18pt" )
        , ( "padding", "2px" )
        , ( "border", "1px solid black" )
        ]
    , rule
        [ Id TopInputId ]
        [ ( "margin-bottom", "0.5em" )
        ]
    , rule
        [ Id FooterId ]
        [ ( "margin-top", "1em" )
        ]
    , rule
        [ Class ControlsClass ]
        [ ( "font-size", "14pt" )
        ]
    , rule
        [ Class BoardCellClass ]
        [ ( "padding", "4px" )
        , ( "textAlign", "center" )
        , ( "border", "1px solid black" )
        ]
    , rule
        [ Class BoardLabelClass ]
        [ ( "padding", "4px" )
        , ( "font-size", "50%" )
        , ( "text-align", "center" )
        , ( "border", "1px solid black" )
        ]
    , rule
        [ Class HelpTextClass ]
        [ ( "width", "80%" )
        , ( "max-width", "40em" )
        ]
    , rule
        [ Class StarMenuButtonClass ]
        [ ( "font-size", "100%" )
        , ( "width", "100%" )
        ]
    , rule [ Class FullWidthClass ]
        [ ( "width", "100%" )
        ]
    , rule
        [ Class ErrorClass ]
        [ ( "color", "red" )
        ]
    , rule
        [ Class PrettyTable ]
        [ ( "margin", "0em 0.5em 0.5em 0.5em" )
        , ( "background", "white" )
        , ( "border-collapse", "collapse" )
        ]
    , rule
        [ Descendant (Type "th") (Class PrettyTable) ]
        [ ( "border", "1px silver solid" )
        , ( "padding", "0.2em" )
        , ( "background", "gainsboro" )
        , ( "text-align", "center" )
        ]
    , rule
        [ Descendant (Type "td") (Class PrettyTable) ]
        [ ( "border", "1px silver solid" )
        , ( "padding", "0.2em" )
        ]
    , rule
        [ Descendant (Type "tr:nth-child(odd)") (Class PrettyTable) ]
        [ ( "background-color", "white" ) -- "#f2f2f2" )
        ]
    ]


stylesheet =
    Css.stylesheet imports rules



-- This is for inclusion at the beginning of the outermost div


style =
    Css.style [ Html.Attributes.style "scoped" "true" ] stylesheet



-- For use in the attributes of Html elements
-- E.g. id Board


id =
    stylesheet.id


class =
    stylesheet.class
