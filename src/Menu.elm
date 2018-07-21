module Menu exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : Html msg
view =
    div []
        [ a [ href "#page1" ] [ text "Counter" ]
        , a [ href "#page2" ] [ text "Text Field" ]
        ]
