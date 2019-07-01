module Init exposing
    ( Flags
    , init
    , resultSearch
    )

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
import Msg
import NaturalOrdering
import Package
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


maybeToBool : Maybe a -> Bool
maybeToBool something =
    case something of
        Just _ ->
            True

        Nothing ->
            False


preparation :
    { cached_sortedKeywordsWithQuantity : List Keywords.WithQuantity
    , cached_sortedPeopleWithQuantity : List People.WithQuantity
    , cached_sortedLinksWithQuantity : List Links.WithQuantity
    , cached_missingPeople : List People.Id
    , cached_missingKeywords : List Keywords.Id
    , cached_indexForPeople : Maybe ( ElmTextSearch.Index People.WithQuantity, List ( Int, String ) )
    , cached_indexForKeywords : Maybe ( ElmTextSearch.Index Keywords.WithQuantity, List ( Int, String ) )
    , cached_indexForLinks : Maybe ( ElmTextSearch.Index Links.WithQuantity, List ( Int, String ) )
    }
preparation =
    let
        --
        -- items with quantities
        --
        keywordsWithQuantity =
            createKeywordsWithQuantity

        peopleWithQuantity =
            createPeopleWithQuantity

        linksWithQuantity =
            createLinksWithQuantity

        --
        -- filtering items that have no links
        --
        keywordsWithQuantity_filtered =
            keywordsWithQuantity
                |> List.filter
                    (\item -> maybeToBool item.maybeLookup)
                |> List.map
                    (\item ->
                        { lookup = Maybe.withDefault Keywords.empty item.maybeLookup
                        , quantity = item.quantity
                        }
                    )

        peopleWithQuantity_filtered =
            peopleWithQuantity
                |> List.filter
                    (\item -> maybeToBool item.maybeLookup)
                |> List.map
                    (\item ->
                        { lookup = Maybe.withDefault People.empty item.maybeLookup
                        , quantity = item.quantity
                        }
                    )

        --
        -- items that have links but don't exists
        --
        missingPeople =
            peopleWithQuantity
                |> List.filter
                    (\item -> not <| maybeToBool item.maybeLookup)
                |> List.map (\item -> item.id)

        missingKeywords =
            keywordsWithQuantity
                |> List.filter
                    (\item -> not <| maybeToBool item.maybeLookup)
                |> List.map (\item -> item.id)

        sortedPeople =
            List.sortWith
                (NaturalOrdering.compareOn (\item -> item.lookup.name))
                peopleWithQuantity_filtered

        sortedKeywords =
            List.sortWith
                (NaturalOrdering.compareOn (\item -> item.lookup.name))
                keywordsWithQuantity_filtered

        sortedLinks =
            List.sortWith
                (NaturalOrdering.compareOn (\item -> item.lookup.name))
                linksWithQuantity
    in
    { cached_sortedKeywordsWithQuantity = sortedKeywords
    , cached_sortedPeopleWithQuantity = sortedPeople
    , cached_sortedLinksWithQuantity = sortedLinks
    , cached_missingPeople = missingPeople
    , cached_missingKeywords = missingKeywords
    , cached_indexForPeople = Just <| indexBuilderForPeople sortedPeople
    , cached_indexForKeywords = Just <| indexBuilderForKeywords sortedKeywords
    , cached_indexForLinks = Just <| indexBuilderForLinks sortedLinks
    }



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
                        (Json.Decode.list Package.decodePackage)
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
                    preparation
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



-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████


createLinksWithQuantity :
    List
        { lookup : Links.Attributes
        , quantity : Int
        }
createLinksWithQuantity =
    List.map
        (\item -> { lookup = item, quantity = 0 })
        Links.list


createKeywordsWithQuantity :
    List
        { id : Keywords.Id
        , maybeLookup : Maybe Keywords.Attributes
        , quantity : Int
        }
createKeywordsWithQuantity =
    Links.list
        |> List.concatMap (\link -> link.keywords)
        |> List.Extra.gatherEquals
        |> List.map
            (\item ->
                { id = Tuple.first item
                , quantity = List.length (Tuple.second item) + 1
                , maybeLookup = List.head <| List.filter (\keyword -> Tuple.first item == keyword.id) Keywords.list
                }
            )


createPeopleWithQuantity :
    List
        { id : People.Id
        , maybeLookup : Maybe People.Attributes
        , quantity : Int
        }
createPeopleWithQuantity =
    Links.list
        |> List.concatMap (\link -> link.authors)
        |> List.Extra.gatherEquals
        |> List.map
            (\item ->
                { id = Tuple.first item
                , quantity = List.length (Tuple.second item) + 1
                , maybeLookup = List.head <| List.filter (\person -> Tuple.first item == person.id) People.list
                }
            )


indexBuilderForLinks :
    List { b | lookup : { a | description : String, name : String } }
    -> ( ElmTextSearch.Index { b | lookup : { a | description : String, name : String } }, List ( Int, String ) )
indexBuilderForLinks list =
    let
        index =
            ElmTextSearch.newWith
                { ref = \item -> item.lookup.name
                , fields =
                    [ ( \item -> item.lookup.name, 5.0 )
                    , ( \item -> item.lookup.description, 1.0 )
                    ]
                , listFields = []
                , indexType = "Elm Resources - Customized Stop Words v1"
                , initialTransformFactories = Index.Defaults.defaultInitialTransformFactories
                , transformFactories = Index.Defaults.defaultTransformFactories
                , filterFactories = [ Shared2.createMyStopWordFilter ]
                }
    in
    ElmTextSearch.addDocs list index


indexBuilderForPeople :
    List { b | lookup : { a | name : String, twitter : String, github : String } }
    -> ( ElmTextSearch.Index { b | lookup : { a | name : String, twitter : String, github : String } }, List ( Int, String ) )
indexBuilderForPeople list =
    let
        index =
            ElmTextSearch.newWith
                { ref = \item -> item.lookup.name
                , fields =
                    [ ( \item -> item.lookup.name, 5.0 )
                    , ( \item -> item.lookup.twitter, 1.0 )
                    , ( \item -> item.lookup.github, 1.0 )
                    ]
                , listFields = []
                , indexType = "Elm Resources - Customized Stop Words v1"
                , initialTransformFactories = Index.Defaults.defaultInitialTransformFactories
                , transformFactories = Index.Defaults.defaultTransformFactories
                , filterFactories = [ Shared2.createMyStopWordFilter ]
                }
    in
    ElmTextSearch.addDocs list index


indexBuilderForKeywords :
    List { b | lookup : { a | name : String } }
    -> ( ElmTextSearch.Index { b | lookup : { a | name : String } }, List ( Int, String ) )
indexBuilderForKeywords list =
    let
        index =
            ElmTextSearch.newWith
                { ref = \item -> item.lookup.name
                , fields =
                    [ ( \item -> item.lookup.name, 5.0 )
                    ]
                , listFields = []
                , indexType = "Elm Resources - Customized Stop Words v1"
                , initialTransformFactories = Index.Defaults.defaultInitialTransformFactories
                , transformFactories = Index.Defaults.defaultTransformFactories
                , filterFactories = [ Shared2.createMyStopWordFilter ]
                }
    in
    ElmTextSearch.addDocs list index


resultSearch :
    ( ElmTextSearch.Index doc, b )
    -> String
    -> Result String ( ElmTextSearch.Index doc, List ( String, Float ) )
resultSearch index searchString =
    ElmTextSearch.search searchString (Tuple.first index)
