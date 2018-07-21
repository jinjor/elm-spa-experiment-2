module Main exposing (..)

import Common exposing (..)
import Header
import Html exposing (..)
import Menu
import Page1 exposing (..)
import Page2 exposing (..)
import Template
import Top


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { hash : String
    , token : Maybe String
    }



-- MODEL


type alias Model =
    { user : User
    , page : Page
    , error : Maybe Error
    }


init : Flags -> ( Model, Cmd Msg )
init { hash, token } =
    let
        user =
            if token == Nothing then
                Guest

            else
                Admin

        page =
            initPage hash user
    in
    ( { page = page
      , user = user
      , error = Nothing
      }
    , Cmd.none
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
    = HashChanged ( String, Maybe String )
    | GotError Error
    | Logout
    | TopMsg Top.Msg
    | Page1Msg Page1.Msg
    | Page2Msg Page2.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( HashChanged ( hash, token ), _ ) ->
            let
                user =
                    if token == Nothing then
                        Guest

                    else
                        Admin

                page =
                    initPage hash user
            in
            ( { model
                | page = page
                , user = user
                , error = Nothing
              }
            , Cmd.none
            )

        ( GotError AuthError, _ ) ->
            ( model
            , logout ()
            )

        ( GotError error, _ ) ->
            ( { model | error = Just error }
            , Cmd.none
            )

        ( Logout, _ ) ->
            ( model
            , logout ()
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


initPage : String -> User -> Page
initPage hash user =
    case ( hash, user ) of
        ( _, Guest ) ->
            Top Top.init

        ( "", _ ) ->
            MainPage Blank

        ( "#", _ ) ->
            MainPage Blank

        ( "#page1", _ ) ->
            MainPage (Page1 Page1.init)

        ( "#page2", _ ) ->
            MainPage (Page2 Page2.init)

        _ ->
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


viewMainPage : Session a -> MainPage -> Html Msg
viewMainPage session page =
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
                Page2.view session sub
                    |> Html.map Page2Msg
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ hashchanges HashChanged
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
