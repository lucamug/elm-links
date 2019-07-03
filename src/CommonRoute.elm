module CommonRoute exposing
    ( fromUrl
    , toStringAndHash
    , urlToMaybeRoute
    )

import Url
import Url.Parser exposing ((</>))


toStringAndHash :
    { b
        | toString : a -> String
        , routeMode : String
    }
    -> a
    -> String
toStringAndHash conf route =
    let
        string =
            conf.toString route
    in
    if conf.routeMode == "path" then
        "/" ++ string

    else
        "#" ++ string



-- INTERNAL


fromUrl :
    { a
        | disabled : b
        , parser : Url.Parser.Parser (b -> b) b
        , routeMode : String
    }
    -> Url.Url
    -> b
fromUrl conf url =
    Maybe.withDefault conf.disabled <| urlToMaybeRoute conf url


urlToMaybeRoute :
    { b
        | parser : Url.Parser.Parser (a -> a) a
        , routeMode : String
    }
    -> Url.Url
    -> Maybe a
urlToMaybeRoute conf url =
    -- We copy the fragment in to the path first because the parser only works
    -- on the path
    if conf.routeMode == "path" then
        Url.Parser.parse conf.parser url

    else
        Url.Parser.parse conf.parser { url | path = Maybe.withDefault "" url.fragment }
