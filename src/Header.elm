module Header exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


view : msg -> Html msg
view logout =
    layout (button [ onClick logout ] [ text "Logout" ])


empty : Html msg
empty =
    layout (text "")


layout : Html msg -> Html msg
layout menu =
    div []
        [ a [ href "#" ] [ text "Sample App" ]
        , menu
        ]
