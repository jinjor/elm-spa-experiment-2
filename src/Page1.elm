module Page1 exposing (..)

import Common exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Task



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement
    | GotSomething ()


update : Msg -> Model -> Return Model Msg
update msg model =
    case msg of
        Increment ->
            return (model + 1)
                |> withTask (Task.map GotSomething badAuth)

        Decrement ->
            return (model - 1)

        GotSomething _ ->
            return model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
