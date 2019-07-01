module Model exposing
    ( ColorMode(..)
    , LayoutMode(..)
    , Model
    )

import Browser.Navigation
import Data.Keywords as Keywords
import Data.Links as Links
import Data.People as People
import ElmTextSearch
import Url


type ColorMode
    = Day
    | Night
    | Green


type LayoutMode
    = List
    | Grid


type alias Model =
    { url : Url.Url
    , key : Browser.Navigation.Key
    , filter : String
    , squareQuantity : Int
    , squareWidth : Float
    , width : Int
    , pageInTopArea : Bool
    , colorMode : ColorMode
    , layoutMode : LayoutMode

    -- Cached stuff
    , cached_sortedKeywordsWithQuantity : List Keywords.WithQuantity
    , cached_sortedPeopleWithQuantity : List People.WithQuantity
    , cached_sortedLinksWithQuantity : List Links.WithQuantity
    , cached_missingPeople : List People.Id
    , cached_missingKeywords : List Keywords.Id
    , cached_indexForPeople : Maybe ( ElmTextSearch.Index People.WithQuantity, List ( Int, String ) )
    , cached_indexForKeywords : Maybe ( ElmTextSearch.Index Keywords.WithQuantity, List ( Int, String ) )
    , cached_indexForLinks : Maybe ( ElmTextSearch.Index Links.WithQuantity, List ( Int, String ) )
    }
