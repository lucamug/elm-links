module Utils exposing
    ( decode
    , encode
    , idToString
    , stringToId
    )

import Url


get : List b -> a -> (b -> a) -> Maybe b
get list query field =
    let
        items =
            List.filter
                (\i -> field i == query)
                list
    in
    List.head items


idToString :
    List { b | id : a, name : String }
    -> a
    -> Maybe String
idToString list id =
    let
        maybeItem =
            get list id .id
    in
    case maybeItem of
        Nothing ->
            Nothing

        Just item ->
            Just <| encode (.name item)


stringToId :
    List { b | id : a, name : String }
    -> String
    -> Maybe a
stringToId list name =
    let
        maybeItem =
            get list (decode name) .name
    in
    case maybeItem of
        Nothing ->
            Nothing

        Just item ->
            Just <| .id item


encode : String -> String
encode string =
    string
        |> String.replace " " "_"
        |> Url.percentEncode


decode : String -> String
decode string =
    string
        |> Url.percentDecode
        |> Maybe.withDefault ""
        |> String.replace "_" " "
