port module Main exposing (..)

import Common exposing (..)
import Html exposing (..)
import Page1 exposing (..)
import Page2 exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { user : User
    , page : Page
    , error : Maybe Error
    }


init : ( Model, Cmd Msg )
init =
    ( { user = Admin
      , page = Blank
      , error = Nothing
      }
    , Cmd.none
    )


type Page
    = Blank
    | NotFound
    | Page1 Page1.Model
    | Page2 Page2.Model



-- UPDATE


type Msg
    = HashChanged String
    | GotError Error
    | Page1Msg Page1.Msg
    | Page2Msg Page2.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( HashChanged hash, _ ) ->
            (case hash of
                "" ->
                    ( Blank, Cmd.none )

                "#" ->
                    ( Blank, Cmd.none )

                "#page1" ->
                    ( Page1 Page1.init, Cmd.none )

                "#page2" ->
                    ( Page2 Page2.init, Cmd.none )

                _ ->
                    ( NotFound, Cmd.none )
            )
                |> Tuple.mapFirst
                    (\page ->
                        { model
                            | page = page
                        }
                    )

        ( GotError error, _ ) ->
            ( { model | error = Just error }
            , Cmd.none
            )

        ( Page1Msg msg, Page1 sub ) ->
            Page1.update msg sub
                |> Tuple.mapFirst (\sub -> { model | page = Page1 sub })
                |> Tuple.mapSecond (Cmd.map Page1Msg)

        ( Page2Msg msg, Page2 sub ) ->
            Page2.update msg sub
                |> toTuple
                    (\sub -> { model | page = Page2 sub })
                    GotError
                    Page2Msg

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.error of
        Just err ->
            text err

        Nothing ->
            case model.page of
                Blank ->
                    text ""

                NotFound ->
                    text "not found"

                Page1 sub ->
                    Page1.view sub
                        |> Html.map Page1Msg

                Page2 sub ->
                    Page2.view model sub
                        |> Html.map Page2Msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ hashchanges HashChanged
        , case model.page of
            Blank ->
                Sub.none

            NotFound ->
                Sub.none

            Page1 sub ->
                Page1.subscriptions sub
                    |> Sub.map Page1Msg

            Page2 sub ->
                Page2.subscriptions sub
                    |> Sub.map Page2Msg
        ]



-- PORTS


port hashchanges : (String -> msg) -> Sub msg
