module Package exposing (Package, decodePackage)

import Array
import Browser.Navigation
import CommonRoute
import Data.Keywords as Keywords
import Data.Links as Links
import Data.People as People
import ElmTextSearch
import Http
import Index.Defaults
import Json.Decode
import Json.Decode.Pipeline
import List.Extra
import Model
import NaturalOrdering
import Route
import Shared2
import Url



-- Packages Data


type alias Package =
    { name : String
    , summary : String
    , license : String
    , versions : List String
    }


decodePackage : Json.Decode.Decoder Package
decodePackage =
    Json.Decode.succeed Package
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "summary" Json.Decode.string
        |> Json.Decode.Pipeline.required "license" Json.Decode.string
        |> Json.Decode.Pipeline.required "versions" (Json.Decode.list Json.Decode.string)



-- Package
--
--     { name : String
--     , summary : String
--     , license : String
--     , versions : List String
--     }
--
--   "name": "ianmackenzie/elm-3d-camera",
--   "summary": "Camera type for 3D rendering and projection",
--   "license": "MPL-2.0",
--   "versions": ["1.1.0"]
--
--
-- Link
--
--     { name : String -- Name of the link, also used as ID
--     , url : String -- Url of the link
--     , code : String -- Url to the source code, if available. Otherwise empty string.
--     , picture : String -- Picture representing the link, if available. Otherwise empty string.
--     , description : String -- Description of the link
--     , keywords : List Keywords.Id -- List of related Keywords, from `src/data/Keywords.elm`
--     , authors : List People.Id -- List of related Authors, from `src/data/People.elm`
--     }


packageToLink : Package -> Links.Attributes
packageToLink package =
    let
        authorAndName =
            String.split "/" package.name

        author =
            Maybe.withDefault "" <| List.head authorAndName

        name =
            Maybe.withDefault "" <| Array.get 1 (Array.fromList authorAndName)
    in
    { name = name -- Name of the link, also used as ID
    , url = "https://package.elm-lang.org/packages/" ++ package.name ++ "/latest/" -- Url of the link
    , code = "https://github.com/" ++ package.name ++ "/" -- Url to the source code, if available. Otherwise empty string.
    , picture = "" -- Picture representing the link, if available. Otherwise empty string.
    , description = package.summary -- Description of the link
    , keywords = [] -- List Keywords.Id -- List of related Keywords, from `src/data/Keywords.elm`
    , authors = [] -- [ author ] -- List of related Authors, from `src/data/People.elm`
    }
