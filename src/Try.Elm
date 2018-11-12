module Try exposing (User, logged, users)

import Dict exposing (Dict)


users : Dict String User
users =
    Dict.fromList
        [ ( "Alice", User "Alice" 28 1.65 )
        , ( "Bob", User "Bob" 19 1.82 )
        , ( "Chuck", User "Chuck" 33 1.75 )
        ]


type alias User =
    { name : String
    , age : Int
    , height : Float
    }


logged =
    Debug.log "Log.." <|
        Dict.insert "Bob" (User "Bob" 19 5.3) users
