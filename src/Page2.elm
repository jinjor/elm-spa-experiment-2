module Page2 exposing (..)

import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task



-- MODEL


type alias Model =
    { content : String
    }


init : Model
init =
    { content = "foo" }



-- UPDATE


type Msg
    = Change String
    | GotData ()


update : Msg -> Model -> Return Model Msg
update msg model =
    case msg of
        Change newContent ->
            return { model | content = newContent }
                |> withTask (Task.map GotData badRequest)

        GotData _ ->
            return model



-- VIEW


view : Context a -> Model -> Html Msg
view { user } model =
    div []
        [ input
            [ placeholder "Text to reverse"
            , onInput Change
            , value model.content
            , readonly (user == Guest)
            ]
            []
        , div [] [ text (String.reverse model.content) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
