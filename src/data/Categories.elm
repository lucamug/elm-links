module Data.Categories exposing
    ( Attributes
    , Id(..)
    , WithQuantity
    , empty
    , emptyWithQuantity
    , idToString
    , list
    , stringToId
    )

{- This file is still Work In Progress, it is not loaded yet -}

import Utils


type alias Attributes =
    { id : Id
    , name : String
    , picture : String
    }


type alias WithQuantity =
    { lookup : Attributes
    , quantity : Int
    }


empty : Attributes
empty =
    { id = Empty
    , name = ""
    , picture = ""
    }


emptyWithQuantity : WithQuantity
emptyWithQuantity =
    { lookup = empty
    , quantity = 0
    }


idToString : Id -> String
idToString id =
    Utils.idToString list empty id


stringToId : String -> Id
stringToId name =
    Utils.stringToId list empty name



{-
   ██████   █████  ████████  █████
   ██   ██ ██   ██    ██    ██   ██
   ██   ██ ███████    ██    ███████
   ██   ██ ██   ██    ██    ██   ██
   ██████  ██   ██    ██    ██   ██
-}


type Id
    = Empty
    | DevTool -- Ellie, Analyse, Elmjutsu
    | Application_Game -- Rubik cube
    | Application_Educational -- Moltiplication table
    | Application_Others -- SPA
    | Book
    | Package
    | Conference_Website
    | Code_as_example -- This can be extrapolated from the code field



{-

   type Media
       = Empty
       | Video
       | Post
       | Application
-}


list : List Attributes
list =
    [ { id = Empty
      , name = "Empty"
      , picture = ""
      }
    ]
