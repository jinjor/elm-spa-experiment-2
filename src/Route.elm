module Route exposing (..)

import Navigation exposing (Location)


type Route
    = Top
    | Page1
    | Page2


fromLocation : Location -> Maybe Route
fromLocation { hash } =
    case hash of
        "" ->
            Just Top

        "#" ->
            Just Top

        "#page1" ->
            Just Page1

        "#page2" ->
            Just Page2

        _ ->
            Nothing


toHash : Route -> String
toHash route =
    case route of
        Top ->
            "#"

        Page1 ->
            "#page1"

        Page2 ->
            "#page2"
