module Common exposing (..)

import Task exposing (Task)


type User
    = Guest
    | Admin


type alias Session a =
    { a
        | user : User
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
