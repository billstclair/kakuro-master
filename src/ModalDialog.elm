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

import Html exposing ( Html, Attribute, div )
import Html.Attributes exposing ( style, attribute )
import Html.Events exposing ( onClick )

outerStyle : List (String, String)
outerStyle = [ ( "position", "fixed" )
             , ( "z-index", "1" )
             , ( "left", "0" )
             , ( "top", "0" )
             , ( "width", "100%" )
             , ( "height", "100%" )
             , ( "overflow", "auto" )
             , ( "background-color", "rgb(0,0,0)" )
             , ( "background-color", "rgba(0,0,0,0.2)" )
             ]

innerStyle : List (String, String)
innerStyle = [ ( "margin", "100px auto" )
             , ( "border", "1px solid #888" )
             , ( "width", "30em" )
             , ( "background-color", "white" )
             ]

modalDiv : msg -> List (Attribute msg) -> List (Attribute msg) -> List (Html msg) -> Html msg
modalDiv closeMsg outerAttributes attributes children =
  div ( List.append
            [ style outerStyle
            , onClick closeMsg
            ]
            outerAttributes
      )
      [ div ( List.append
                  [ style innerStyle
                  , attribute "onClick" "event.stopPropagation()"
                  ]
                  attributes
            )
            children
      ]
