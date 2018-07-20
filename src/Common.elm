module Common exposing (..)


type User
    = Guest
    | Admin


type alias Session a =
    { a
        | user : User
    }
