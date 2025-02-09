----------------------------------------------------------------------
--
-- ModalDialog.elm
-- Modal dialogs.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
-- Patterned after http://www.w3schools.com/howto/tryit.asp?filename=tryhow_css_modal
-- See ../site/modal.html and modal-example.elm
--
----------------------------------------------------------------------


module ModalDialog exposing (modalDiv)

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (attribute, style)
import Html.Events exposing (onClick)


outerStyle : List (Attribute msg)
outerStyle =
    [ style "position" "fixed"
    , style "z-index" "1"
    , style "left" "0"
    , style "top" "0"
    , style "width" "100%"
    , style "height" "100%"
    , style "overflow" "auto"
    , style "background-color" "rgb(0,0,0)"
    , style "background-color" "rgba(0,0,0,0.2)"
    ]


innerStyle : List (Attribute msg)
innerStyle =
    [ style "margin" "100px auto"
    , style "border" "1px solid #888"
    , style "width" "20em"
    , style "background-color" "white"
    ]


modalDiv : msg -> List (Attribute msg) -> List (Attribute msg) -> List (Html msg) -> Html msg
modalDiv closeMsg outerAttributes attributes children =
    div
        (List.concat
            [ outerStyle
            , [ onClick closeMsg ]
            , outerAttributes
            ]
        )
        [ div
            (List.concat
                [ innerStyle
                , [ attribute "onClick" "event.stopPropagation()" ]
                , attributes
                ]
            )
            children
        ]
