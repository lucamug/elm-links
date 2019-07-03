module Preparation exposing (preparation)

import Browser.Navigation
import CommonRoute
import Data.Keywords as Keywords
import Data.Links as Links
import Data.Packages as Packages
import Data.People as People
import Dict
import ElmTextSearch
import Http
import Index.Defaults
import Json.Decode
import Json.Decode.Pipeline
import List.Extra
import Model
import Msg
import NaturalOrdering
import Route
import Shared2
import Url


preparation :
    { a
        | links : List Links.Attributes
        , keywords : List Keywords.Attributes
        , people : List People.Attributes
    }
    ->
        { cached_sortedKeywordsWithQuantity : List Keywords.WithQuantity
        , cached_sortedPeopleWithQuantity : List People.WithQuantity
        , cached_sortedLinksWithQuantity : List Links.WithQuantity
        , cached_missingPeople : List String
        , cached_missingKeywords : List String
        , cached_indexForPeople : Maybe ( ElmTextSearch.Index People.WithQuantity, List ( Int, String ) )
        , cached_indexForKeywords : Maybe ( ElmTextSearch.Index Keywords.WithQuantity, List ( Int, String ) )
        , cached_indexForLinks : Maybe ( ElmTextSearch.Index Links.WithQuantity, List ( Int, String ) )
        }
preparation { links, keywords, people } =
    let
        --
        -- dicts for fast lookup
        --
        keywordsDict =
            Dict.fromList <| List.map (\keyword -> ( String.toLower keyword.id, keyword )) keywords

        peopleDict =
            Dict.fromList <| List.map (\person -> ( String.toLower person.id, person )) people

        peopleDictByGithub =
            Dict.fromList <| List.map (\person -> ( String.toLower person.github, person.id )) (List.filter (\person -> person.github /= "") people)

        --
        -- items with quantities
        --
        keywordsWithQuantity =
            createKeywordsWithQuantity links keywordsDict

        peopleWithQuantity =
            createPeopleWithQuantity links peopleDict peopleDictByGithub

        linksWithQuantity =
            createLinksWithQuantity links

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

        --
        -- sorting
        --
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



-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████


maybeToBool : Maybe a -> Bool
maybeToBool something =
    case something of
        Just _ ->
            True

        Nothing ->
            False


createLinksWithQuantity :
    List Links.Attributes
    ->
        List
            { lookup : Links.Attributes
            , quantity : Int
            }
createLinksWithQuantity links =
    List.map
        (\item -> { lookup = item, quantity = 0 })
        links


createKeywordsWithQuantity :
    List Links.Attributes
    -> Dict.Dict String Keywords.Attributes
    ->
        List
            { id : String
            , maybeLookup : Maybe Keywords.Attributes
            , quantity : Int
            }
createKeywordsWithQuantity links keywordsDict =
    -- List of links
    links
        -- ["SPA","Boilerplate","JSON",..]
        |> List.concatMap (\link -> List.map (\keyword -> String.toLower keyword) link.keywords)
        -- [("SPA",["SPA","SPA"]),("Boilerplate",["Boilerplate"..
        |> List.Extra.gatherEquals
        -- [ { id = "SPA"
        --   , maybeLookup = Just { id = "SPA", name = "SPA", picture = "" }
        --   , quantity = 6
        --   } ,
        --   { id = "Boilerplate"
        --   ..
        |> List.map
            (\item ->
                let
                    keywordToLookup =
                        Tuple.first item

                    searchResult =
                        Dict.get (String.toLower keywordToLookup) keywordsDict
                in
                { id = keywordToLookup
                , quantity = List.length (Tuple.second item) + 1
                , maybeLookup =
                    case searchResult of
                        Nothing ->
                            Just
                                -- creating a password if it doesn't exsist
                                { id = keywordToLookup
                                , name = keywordToLookup
                                , picture = ""
                                }

                        Just keyword ->
                            Just keyword
                }
            )


createPeopleWithQuantity :
    List Links.Attributes
    -> Dict.Dict String People.Attributes
    -> Dict.Dict String String
    ->
        List
            { id : String
            , maybeLookup : Maybe People.Attributes
            , quantity : Int
            }
createPeopleWithQuantity links peopleDict peopleDictByGithub =
    links
        -- Putting all authors in an array, with repetitions
        |> List.concatMap (\link -> link.authors)
        -- Looking up person.id as if it was github handle
        -- |> List.map (\personId -> Maybe.withDefault personId <| Dict.get (String.toLower personId) peopleDictByGithub)
        -- Grouping similar person.id together to count them
        |> List.Extra.gatherEquals
        |> List.map
            (\item ->
                let
                    peopleToLookup =
                        Tuple.first item

                    searchResult =
                        Dict.get (String.toLower peopleToLookup) peopleDict
                in
                { id = Tuple.first item
                , quantity = List.length (Tuple.second item) + 1

                -- , maybeLookup = List.head <| List.filter (\person -> Tuple.first item == person.id) people
                , maybeLookup =
                    case searchResult of
                        Nothing ->
                            Just
                                -- creating a person if it doesn't exsist, using the peopleToLookup
                                -- as id, name and github handle
                                { id = peopleToLookup
                                , name = peopleToLookup -- Name, usually the same as the type constructor
                                , picture = "" -- Picture that represent the person
                                , twitter = "" -- Twitter handle
                                , github = peopleToLookup -- Github handle
                                , url = "" -- Link to a personal homepage
                                , country = ""
                                }

                        Just person ->
                            Just person
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
