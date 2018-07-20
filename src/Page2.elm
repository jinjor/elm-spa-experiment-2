module Page2 exposing (..)

import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( { model | content = newContent }, Cmd.none )



-- VIEW


view : Session a -> Model -> Html Msg
view session model =
    div []
        [ input
            [ placeholder "Text to reverse"
            , onInput Change
            , value model.content
            , readonly (session.user == Guest)
            ]
            []
        , div [] [ text (String.reverse model.content) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
