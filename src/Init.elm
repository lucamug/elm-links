module Init exposing
    ( Flags
    , init
    )

import Browser.Navigation
import CommonRoute
import Data.Keywords as Keywords
import Data.Links as Links
import Data.Packages as Packages
import Data.People as People
import ElmTextSearch
import Http
import Index.Defaults
import Json.Decode
import Json.Decode.Pipeline
import List.Extra
import Model
import Msg
import NaturalOrdering
import Preparation
import Route
import Shared2
import Url



-- CONSTANTS


initialSquareWidth : Int
initialSquareWidth =
    56


initialSquareWidthForMobile : Int
initialSquareWidthForMobile =
    50



-- INIT


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model.Model, Cmd Msg.Msg )
init flags url key =
    let
        getPackagesCmd =
            Http.get
                { url = "data/search.json"
                , expect =
                    Http.expectJson
                        Msg.GotPackages
                        (Json.Decode.list Packages.decodeAttributes)
                }

        model =
            { url = url
            , key = key
            , filter = filter
            , squareQuantity = squareQuantity
            , squareWidth = toFloat flags.width / toFloat squareQuantity
            , width = flags.width
            , pageInTopArea = True
            , colorMode = Model.Day
            , layoutMode = Model.Grid

            -- Cached stuff
            , cached_sortedKeywordsWithQuantity = []
            , cached_sortedPeopleWithQuantity = []
            , cached_sortedLinksWithQuantity = []
            , cached_missingPeople = []
            , cached_missingKeywords = []
            , cached_indexForPeople = Nothing
            , cached_indexForKeywords = Nothing
            , cached_indexForLinks = Nothing
            }

        model2 =
            let
                p =
                    Preparation.preparation { links = Links.list, keywords = Keywords.list, people = People.list }
            in
            { model
                | cached_sortedKeywordsWithQuantity = p.cached_sortedKeywordsWithQuantity
                , cached_sortedPeopleWithQuantity = p.cached_sortedPeopleWithQuantity
                , cached_sortedLinksWithQuantity = p.cached_sortedLinksWithQuantity
                , cached_missingPeople = p.cached_missingPeople
                , cached_missingKeywords = p.cached_missingKeywords
                , cached_indexForPeople = p.cached_indexForPeople
                , cached_indexForKeywords = p.cached_indexForKeywords
                , cached_indexForLinks = p.cached_indexForLinks
            }

        squareQuantity =
            if flags.width < 475 then
                flags.width // initialSquareWidthForMobile

            else
                flags.width // initialSquareWidth

        filter =
            case CommonRoute.fromUrl Route.conf url of
                Route.Filter filter_ ->
                    filter_

                _ ->
                    ""
    in
    ( model2
    , getPackagesCmd
    )



-- FLAGS


type alias Flags =
    { width : Int
    }
