----------------------------------------------------------------------
--
-- modal-example.elm
-- Example for ModalDialog.elm
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

import ModalDialog exposing (modalDiv)

import Html exposing (Html, button, div, text, span)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, align)

main =
    Html.beginnerProgram { model = model, view = view, update = update }

type alias Model =
    { message : String
    , isDialog : Bool
    }

model : Model
model =
    { message = "Nothing yet."
    , isDialog = False
    }

type Msg
    = Set String
    | OpenDialog
    | CloseDialog
    | Nop

update : Msg -> Model -> Model
update msg model =
    case msg of
        Set string ->
            { model
                | message = string
                , isDialog = False}
        OpenDialog ->
            { model | isDialog = True }
        CloseDialog ->
            { model | isDialog = False }
        Nop ->
            model

br : Html Msg
br =
    Html.br [] []

view : Model -> Html Msg
view model =
    div [ style [ ( "padding", "20px" )
                , ( "width", "30em" )
                , ( "border", "1px solid blue" )
                ]
        ]
        [ text model.message
        , br
        , button [ onClick OpenDialog ]
            [ text "Open" ]
        , dialog model
        ]

fullWidth : Html.Attribute Msg
fullWidth =
    style [ ("width", "100%") ]

dialog : Model -> Html Msg
dialog model =
    if not model.isDialog then
        span [][]
    else
        modalDiv
            CloseDialog
            []
            [ style [ ("padding", "5px")
                    , ("width", "10em")
                    ]
            --, align "center"
            ]
            [ button [ onClick (Set "Hello")
                     , fullWidth
                     ]
                  [ text "Hello" ]
            , br
            , button [ onClick (Set "World")
                     , fullWidth
                     ]
                  [ text "World" ]
            ]
