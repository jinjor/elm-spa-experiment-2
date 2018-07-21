module Top exposing (..)

import Common exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Task



-- MODEL


type alias Model =
    {}


init : Model
init =
    {}



-- UPDATE


type Msg
    = Login
    | GotToken String


update : Msg -> Model -> Return Model Msg
update msg model =
    case msg of
        Login ->
            return model
                |> withTask (Task.map GotToken getToken)

        GotToken token ->
            return model
                |> withCommand (login token)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Login ] [ text "Login" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
