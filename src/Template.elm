module Template exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


headerOnly : Html msg -> Html msg -> Html msg
headerOnly header content =
    div []
        [ header
        , content
        ]


headerAndMenu : Html msg -> Html msg -> Html msg -> Html msg
headerAndMenu header menu content =
    div []
        [ header
        , menu
        , content
        ]
