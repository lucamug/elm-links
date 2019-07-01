module Shared2 exposing (createMyStopWordFilter)

import Browser
import Browser.Navigation
import CommonRoute
import Data.Keywords as Keywords
import Data.Links as Links
import Data.People as People
import ElmTextSearch
import Index.Defaults
import Keyboard
import List.Extra
import NaturalOrdering
import Route
import StopWordFilter
import Url
import Utils


createMyStopWordFilter =
    {- The type signature for this function would be:

       createMyStopWordFilter : Index.Model.Index doc -> ( Index.Model.Index doc, String -> Bool )

       but these types are not exposed.
    -}
    StopWordFilter.createFilterFunc
        []
