module Data.Keywords exposing
    ( Attributes
    , Id(..)
    , WithQuantity
    , empty
    , idToString
    , list
    , stringToId
    )

import Utils


type alias Attributes =
    { id : Id -- The type constructor
    , name : String -- Name, usually the same as the type constructor
    , picture : String -- Picture that represent the keyword
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


idToString : Id -> Maybe String
idToString id =
    Utils.idToString list id


stringToId : String -> Maybe Id
stringToId name =
    Utils.stringToId list name



{-
   ██████   █████  ████████  █████
   ██   ██ ██   ██    ██    ██   ██
   ██   ██ ███████    ██    ███████
   ██   ██ ██   ██    ██    ██   ██
   ██████  ██   ██    ██    ██   ██
-}


type Id
    = Empty
    | Game
    | Mathematics
    | Conference
    | UI
    | Particles
    | Water
    | Confetti
    | Firework
    | Animation
    | Graphs
    | Tangram
    | Elm_ui
    | SPA
    | Boilerplate
    | Rubik
    | WebGL
    | ElmEurope2017
    | ElmEurope2018
    | ElmEurope2019
    | OsloElmDay2017
    | OsloElmDay2019
    | ElmConf2016
    | ElmConf2017
    | ElmConf2018
    | Educational
    | Docker
    | Elixir
    | HTML
    | JSON
    | Converter
    | Swagger
    | SVG
    | Ford
    | Microsoft
    | Font
    | Atom
    | Book
    | CSS
    | ElmUI_Example
    | IBM
    | ListOfLinks
    | IE
    | Performance
    | DevTool
    | Intellij
    | IDEs
    | Benchmark
    | Draggable
    | ProtoBuf
    | GraphQL


list : List Attributes
list =
    [ { id = Draggable
      , name = "Draggable - Drag and Drop"
      , picture = ""
      }
    , { id = ProtoBuf
      , name = "ProtoBuf"
      , picture = ""
      }
    , { id = GraphQL
      , name = "GraphQL"
      , picture = ""
      }
    , { id = DevTool
      , name = "Development Tool - Utility"
      , picture = ""
      }
    , { id = Benchmark
      , name = "Benchmark"
      , picture = ""
      }
    , { id = Intellij
      , name = "IntelliJ"
      , picture = ""
      }
    , { id = IDEs
      , name = "Integrated Development Environment (IDE)"
      , picture = ""
      }
    , { id = Empty
      , name = "Empty"
      , picture = ""
      }
    , { id = Performance
      , name = "Performance"
      , picture = ""
      }
    , { id = IE
      , name = "Microsoft Internet Explorer (IE)"
      , picture = "img/ie.png"
      }
    , { id = IBM
      , name = "IBM"
      , picture = "img/ibm.png"
      }
    , { id = ElmUI_Example
      , name = "elm-ui Examples"
      , picture = ""
      }
    , { id = CSS
      , name = "CSS"
      , picture = "svg/css.svg"
      }
    , { id = SVG
      , name = "SVG"
      , picture = "svg/svg.svg"
      }
    , { id = Swagger
      , name = "Swagger"
      , picture = "img/swagger.png"
      }
    , { id = Converter
      , name = "Converter"
      , picture = ""
      }
    , { id = JSON
      , name = "JSON"
      , picture = "svg/json.svg"
      }
    , { id = HTML
      , name = "HTML"
      , picture = "svg/html.svg"
      }
    , { id = Game
      , name = "Game"
      , picture = "svg/joystick.svg"
      }
    , { id = Mathematics
      , name = "Mathematics"
      , picture = "svg/calculator.svg"
      }
    , { id = Conference
      , name = "Conference"
      , picture = "svg/presentation.svg"
      }
    , { id = UI
      , name = "UI"
      , picture = ""
      }
    , { id = Particles
      , name = "Particles"
      , picture = ""
      }
    , { id = Water
      , name = "Water"
      , picture = "svg/water.svg"
      }
    , { id = Confetti
      , name = "Confetti"
      , picture = "svg/confetti.svg"
      }
    , { id = Firework
      , name = "Firework"
      , picture = "svg/fireworks.svg"
      }
    , { id = Animation
      , name = "Animation"
      , picture = ""
      }
    , { id = Graphs
      , name = "Graphs"
      , picture = ""
      }
    , { id = Tangram
      , name = "Tangram"
      , picture = "svg/tangram.svg"
      }
    , { id = Elm_ui
      , name = "elm-ui"
      , picture = "svg/elephant.svg"
      }
    , { id = SPA
      , name = "SPA"
      , picture = ""
      }
    , { id = Boilerplate
      , name = "Boilerplate"
      , picture = ""
      }
    , { id = Rubik
      , name = "Rubik"
      , picture = "svg/rubik.svg"
      }
    , { id = WebGL
      , name = "WebGL"
      , picture = "svg/webgl.svg"
      }
    , { id = OsloElmDay2017
      , name = "Oslo Elm Day 2017"
      , picture = "img/oslo-elm-day-2017.png"
      }
    , { id = OsloElmDay2019
      , name = "Oslo Elm Day 2019"
      , picture = "img/oslo-elm-day-2019.png"
      }
    , { id = ElmConf2016
      , name = "Elm Conf 2016"
      , picture = "img/elm-conf-2016.png"
      }
    , { id = ElmConf2017
      , name = "Elm Conf 2017"
      , picture = "img/elm-conf-2017.png"
      }
    , { id = ElmConf2018
      , name = "Elm Conf 2018"
      , picture = "img/elm-conf-2018.png"
      }
    , { id = ElmEurope2017
      , name = "Elm Europe 2017"
      , picture = "img/elm-europe-2017.png"
      }
    , { id = ElmEurope2018
      , name = "Elm Europe 2018"
      , picture = "img/elm-europe-2018.png"
      }
    , { id = ElmEurope2019
      , name = "Elm Europe 2019"
      , picture = "img/elm-europe-2019.png"
      }
    , { id = Educational
      , name = "Educational"
      , picture = "svg/educational.svg"
      }
    , { id = Docker
      , name = "Docker"
      , picture = "svg/docker.svg"
      }
    , { id = Font
      , name = "Font"
      , picture = "svg/font.svg"
      }
    , { id = Elixir
      , name = "Elixir"
      , picture = "svg/elixir.svg"
      }
    , { id = Ford
      , name = "Ford"
      , picture = "img/ford.jpeg"
      }
    , { id = Atom
      , name = "Atom"
      , picture = "svg/atom.svg"
      }
    , { id = Book
      , name = "Book"
      , picture = "svg/book.svg"
      }
    , { id = Microsoft
      , name = "Microsoft"
      , picture = "img/microsoft.png"
      }
    , { id = ListOfLinks
      , name = "List of Links"
      , picture = ""
      }
    ]
