port module Common exposing (..)

import Json.Decode
import Json.Encode
import Navigation
import Route
import Task exposing (Task)


type alias Json =
    Json.Encode.Value


type User
    = Guest
    | Admin


type alias Context a =
    { a
        | user : User
    }


type alias Session =
    { token : Maybe String
    }


type Error
    = NetworkError
    | AuthError
    | Bug


badRequest : Task Error ()
badRequest =
    Task.fail Bug


badAuth : Task Error ()
badAuth =
    Task.fail AuthError


getToken : Task Error String
getToken =
    Task.succeed "token"



-- UTILITY


type alias Return model msg =
    { model : model
    , tasks : List (Task Error msg)
    , commands : List (Cmd msg)
    }


return : model -> Return model msg
return model =
    { model = model, tasks = [], commands = [] }


withTask : Task Error msg -> Return model msg -> Return model msg
withTask task ret =
    { ret | tasks = ret.tasks ++ [ task ] }


withCommand : Cmd msg -> Return model msg -> Return model msg
withCommand cmd ret =
    { ret | commands = ret.commands ++ [ cmd ] }


toTuple :
    (model -> parentModel)
    -> (Error -> parentMsg)
    -> (msg -> parentMsg)
    -> Return model msg
    -> ( parentModel, Cmd parentMsg )
toTuple transformModel transformError transformMsg return =
    ( transformModel return.model
    , Cmd.batch
        (List.map
            (Task.attempt
                (\result ->
                    case result of
                        Ok msg ->
                            transformMsg msg

                        Err e ->
                            transformError e
                )
            )
            return.tasks
            ++ List.map (Cmd.map transformMsg) return.commands
        )
    )



-- PORTS


port sessionChanges : (Json -> msg) -> Sub msg


port setSession : Maybe Session -> Cmd msg


login : String -> Cmd msg
login token =
    setSession (Just { token = Just token })


logout : Cmd msg
logout =
    Cmd.batch
        [ setSession Nothing
        , Navigation.modifyUrl (Route.toHash Route.Top)
        ]


decodeSession : Json.Decode.Decoder Session
decodeSession =
    Json.Decode.map Session
        (Json.Decode.maybe (Json.Decode.field "token" Json.Decode.string))
        |> Json.Decode.maybe
        |> Json.Decode.map (Maybe.withDefault { token = Nothing })
