module CommonRoute exposing
    ( emptyUrl
    , fromHrefOrFragmentToMaybeUrl
    , fromHrefToUrl
    , fromMaybeUrl
    , fromStringToUrl
    , fromUrl
    , toStringAndHash
    , toUrl
    , urlToMaybeRoute
    , widgetActive
    )

import Url
import Url.Parser exposing ((</>))


emptyUrl : Url.Url
emptyUrl =
    { protocol = Url.Https
    , host = ""
    , port_ = Nothing
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }


fromHrefToUrl : String -> Url.Url
fromHrefToUrl href =
    Maybe.withDefault emptyUrl <| Url.fromString href


fromFragmentToMaybeUrl : Url.Url -> String -> Maybe Url.Url
fromFragmentToMaybeUrl url fragment =
    Just { url | fragment = Just fragment }


fromHrefOrFragmentToMaybeUrl : Url.Url -> String -> Maybe Url.Url
fromHrefOrFragmentToMaybeUrl url hrefOrFragment =
    -- Elm url parser is not able to parse just fragments, so we need
    -- to use the present maybeUrl in that case.
    -- We can send either the href or just the fragment (with "#") to
    -- Javascript, it is able to handle both
    if String.left 1 hrefOrFragment == "#" then
        -- Case  when it is a fragment
        fromFragmentToMaybeUrl url <| String.dropLeft 1 hrefOrFragment

    else
        -- Case  when it is href
        Url.fromString hrefOrFragment


toStringAndHash : { b | toString : a -> String } -> a -> String
toStringAndHash conf route =
    let
        string =
            conf.toString route
    in
    "#" ++ string


toUrl : { b | toString : a -> String } -> Url.Url -> a -> Url.Url
toUrl conf url route =
    fromStringToUrl url (conf.toString route)


fromStringToUrl : Url.Url -> String -> Url.Url
fromStringToUrl url string =
    { url | fragment = Just string }


fromMaybeUrl :
    { a | defaultRoute : b, disabled : b, parser : Url.Parser.Parser (b -> b) b }
    -> Maybe Url.Url
    -> b
fromMaybeUrl conf maybeUrl =
    case maybeUrl of
        Just url ->
            fromUrl conf url

        Nothing ->
            conf.defaultRoute



-- INTERNAL


fromUrl :
    { a | disabled : b, parser : Url.Parser.Parser (b -> b) b }
    -> Url.Url
    -> b
fromUrl conf url =
    Maybe.withDefault conf.disabled <| urlToMaybeRoute conf url


urlToMaybeRoute :
    { b | parser : Url.Parser.Parser (a -> a) a }
    -> Url.Url
    -> Maybe a
urlToMaybeRoute conf url =
    -- We copy the fragment in to the path first because the parser only works
    -- on the path
    Url.Parser.parse conf.parser { url | path = Maybe.withDefault "" url.fragment }


widgetActive : { b | parser : Url.Parser.Parser (a -> a) a } -> Url.Url -> Bool
widgetActive conf url =
    case urlToMaybeRoute conf url of
        Just _ ->
            True

        Nothing ->
            False
