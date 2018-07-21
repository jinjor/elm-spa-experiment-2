module Main exposing (..)

import Common exposing (..)
import Header
import Html exposing (..)
import Json.Decode
import Menu
import Navigation exposing (Location)
import Page1 exposing (..)
import Page2 exposing (..)
import Route exposing (Route)
import Template
import Top


main : Program Flags Model Msg
main =
    Navigation.programWithFlags
        (Route.fromLocation >> NewRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { session : Json
    }



-- MODEL


type alias Model =
    { route : Maybe Route
    , user : User
    , page : Page
    , error : Maybe Error
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        route =
            Route.fromLocation location

        ( user, cmd ) =
            case Json.Decode.decodeValue decodeSession flags.session of
                Ok session ->
                    ( if session.token == Nothing then
                        Guest

                      else
                        Admin
                    , Cmd.none
                    )

                Err _ ->
                    ( Guest, logout )

        page =
            initPage route user
    in
    ( { route = route
      , page = page
      , user = user
      , error = Nothing
      }
    , cmd
    )


type Page
    = NotFound
    | Top Top.Model
    | MainPage MainPage


type MainPage
    = Blank
    | Page1 Page1.Model
    | Page2 Page2.Model



-- UPDATE


type Msg
    = NewRoute (Maybe Route)
    | SessionChanged Json
    | GotError Error
    | Logout
    | TopMsg Top.Msg
    | Page1Msg Page1.Msg
    | Page2Msg Page2.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( NewRoute route, _ ) ->
            ( { model
                | route = route
                , page = initPage route model.user
                , error = Nothing
              }
            , Cmd.none
            )

        ( SessionChanged json, _ ) ->
            case Json.Decode.decodeValue decodeSession json of
                Ok session ->
                    let
                        user =
                            if session.token == Nothing then
                                Guest

                            else
                                Admin
                    in
                    ( { model
                        | page = initPage model.route user
                        , user = user
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, logout )

        ( GotError AuthError, _ ) ->
            ( model
            , logout
            )

        ( GotError error, _ ) ->
            ( { model | error = Just error }
            , Cmd.none
            )

        ( Logout, _ ) ->
            ( model
            , logout
            )

        ( TopMsg msg, Top sub ) ->
            Top.update msg sub
                |> toTuple
                    (\sub -> { model | page = Top sub })
                    GotError
                    TopMsg

        ( Page1Msg msg, MainPage (Page1 sub) ) ->
            Page1.update msg sub
                |> toTuple
                    (\sub -> { model | page = MainPage (Page1 sub) })
                    GotError
                    Page1Msg

        ( Page2Msg msg, MainPage (Page2 sub) ) ->
            Page2.update msg sub
                |> toTuple
                    (\sub -> { model | page = MainPage (Page2 sub) })
                    GotError
                    Page2Msg

        _ ->
            ( model, Cmd.none )


initPage : Maybe Route -> User -> Page
initPage route user =
    case ( route, user ) of
        ( _, Guest ) ->
            Top Top.init

        ( Just Route.Top, _ ) ->
            MainPage Blank

        ( Just Route.Page1, _ ) ->
            MainPage (Page1 Page1.init)

        ( Just Route.Page2, _ ) ->
            MainPage (Page2 Page2.init)

        ( Nothing, _ ) ->
            NotFound



-- VIEW


view : Model -> Html Msg
view model =
    case model.error of
        Just err ->
            Template.headerOnly
                Header.empty
                (text (toString err))

        Nothing ->
            case model.page of
                NotFound ->
                    Template.headerOnly
                        Header.empty
                        (text "not found")

                Top sub ->
                    Template.headerOnly
                        Header.empty
                        (Top.view sub
                            |> Html.map TopMsg
                        )

                MainPage page ->
                    viewMainPage model page


viewMainPage : Context a -> MainPage -> Html Msg
viewMainPage context page =
    Template.headerAndMenu
        (Header.view Logout)
        Menu.view
        (case page of
            Blank ->
                text ""

            Page1 sub ->
                Page1.view sub
                    |> Html.map Page1Msg

            Page2 sub ->
                Page2.view context sub
                    |> Html.map Page2Msg
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ sessionChanges SessionChanged
        , case model.page of
            NotFound ->
                Sub.none

            Top sub ->
                Top.subscriptions sub
                    |> Sub.map TopMsg

            MainPage Blank ->
                Sub.none

            MainPage (Page1 sub) ->
                Page1.subscriptions sub
                    |> Sub.map Page1Msg

            MainPage (Page2 sub) ->
                Page2.subscriptions sub
                    |> Sub.map Page2Msg
        ]
