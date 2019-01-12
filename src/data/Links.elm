module Data.Links exposing
    ( Attributes
    , WithQuantity
    , list
    )

import Data.Keywords as Keywords exposing (Id(..))
import Data.People as People exposing (Id(..))


type alias Attributes =
    { name : String -- Name of the link, also used as ID
    , url : String -- Url of the link
    , code : String -- Url to the source code, if available. Otherwise empty string.
    , picture : String -- Picture representing the link, if available. Otherwise empty string.
    , description : String -- Description of the link
    , keywords : List Keywords.Id -- List of related Keywords, from `src/data/Keywords.elm`
    , authors : List People.Id -- List of related Authors, from `src/data/People.elm`
    }


type alias WithQuantity =
    { lookup : Attributes
    , quantity : Int
    }



{-
   ██████   █████  ████████  █████
   ██   ██ ██   ██    ██    ██   ██
   ██   ██ ███████    ██    ███████
   ██   ██ ██   ██    ██    ██   ██
   ██████  ██   ██    ██    ██   ██
-}


list : List Attributes
list =
    [ { name = "JSON Tree View"
      , code = "https://github.com/Microsoft/elm-json-tree-view"
      , url = "https://microsoft.github.io/elm-json-tree-view/example/index.html"
      , picture = ""
      , description = "This library provides a JSON tree view. You feed it JSON, and it transforms it into interactive HTML."
      , keywords = [ JSON, Microsoft ]
      , authors = [ Keith_Lazuka ]
      }
    , { name = "HTML/SVG to Elm"
      , code = "https://github.com/isomoar/html-to-elm"
      , url = "http://html-to-elm.herokuapp.com/"
      , picture = ""
      , description = ""
      , keywords = [ HTML, SVG, Converter ]
      , authors = [ Jane_G ]
      }
    , { name = "Swagger to Elm"
      , code = "https://github.com/ahultgren/swagger-elm"
      , url = "http://andreashultgren.se/swagger-elm/"
      , picture = ""
      , description = ""
      , keywords = [ JSON, Converter, Swagger ]
      , authors = [ Andreas_Hultgren ]
      }
    , { name = "JSON to Elm"
      , code = ""
      , url = "http://eeue56.github.io/json-to-elm/"
      , picture = ""
      , description = ""
      , keywords = [ JSON, Converter ]
      , authors = [ Noah_Hall ]
      }
    , { name = "Moltiplication Table"
      , code = "https://github.com/HappMacDonald/MultiplicationTable"
      , url = "http://lightsecond.com/MultiplicationTable/"
      , picture = ""
      , description = "A simple Multiplication Table app to help children work out their math facts"
      , keywords = [ Game, Mathematics, Educational ]
      , authors = [ Jesse_Thompson ]
      }
    , { name = "Oslo Elm Day 2017 - Conference Website"
      , code = ""
      , url = "https://2017.osloelmday.no/"
      , picture = "img/oslo-elm-day-2017.png"
      , description = ""
      , keywords = [ Conference, OsloElmDay2017 ]
      , authors = []
      }
    , { name = "Oslo Elm Day 2019 - Conference Website"
      , code = ""
      , url = "https://osloelmday.no/"
      , picture = "img/oslo-elm-day-2019.png"
      , description = ""
      , keywords = [ Conference, OsloElmDay2019 ]
      , authors = []
      }
    , { name = "elm-conf 2016 - Conference Website"
      , code = ""
      , url = "https://2016.elm-conf.us/"
      , picture = "img/elm-conf-2016.png"
      , description = ""
      , keywords = [ Conference, ElmConf2016 ]
      , authors = []
      }
    , { name = "elm-conf 2017 - Conference Website"
      , code = ""
      , url = "https://2017.elm-conf.us/"
      , picture = "img/elm-conf-2017.png"
      , description = ""
      , keywords = [ Conference, ElmConf2017 ]
      , authors = []
      }
    , { name = "elm-conf 2018 - Conference Website"
      , code = ""
      , url = "https://2018.elm-conf.us/"
      , picture = "img/elm-conf-2018.png"
      , description = ""
      , keywords = [ Conference, ElmConf2018 ]
      , authors = []
      }
    , { name = "Elm Europe 2017 - Conference Website"
      , code = ""
      , url = "https://2017.elmeurope.org/"
      , picture = "img/elm-europe-2017.png"
      , description = ""
      , keywords = [ Conference, ElmEurope2017 ]
      , authors = []
      }
    , { name = "Elm Europe 2018 - Conference Website"
      , code = ""
      , url = "https://2018.elmeurope.org/"
      , picture = "img/elm-europe-2018.png"
      , description = "Elm Europe is a two-day conference dedicated to Elm that took place at the EFREI Engineering School in Villejuif (near Paris, France) on July, 5-6th 2018."
      , keywords = [ Conference, ElmEurope2018 ]
      , authors = []
      }
    , { name = "UI Cards"
      , code = ""
      , url = "https://korban.net/elm/uicards/"
      , picture = ""
      , description = "UICards are a tool for live UI development"
      , keywords = [ UI ]
      , authors = []
      }
    , { name = "elm-particle"
      , code = "https://package.elm-lang.org/packages/BrianHicks/elm-particle/latest/"
      , url = "https://brianhicks.github.io/elm-particle/Confetti.html"
      , picture = "img/elm-particle.png"
      , description = "Simple particle system for web apps. Designed for visual flourishes like confetti."
      , keywords = [ Animation, Confetti, Water, Firework ]
      , authors = [ Brian_Hicks ]
      }
    , { name = "Elm Module Dependency Graph"
      , code = "https://package.elm-lang.org/packages/Brian_Hicks/elm-particle/latest/"
      , url = "https://erkal.github.io/kite/"
      , picture = ""
      , description = "Visualization of the Module Dependency Graph"
      , keywords = [ Animation, Graphs ]
      , authors = []
      }
    , { name = "Elm SPA Example"
      , code = "https://github.com/rtfeldman/elm-spa-example"
      , url = "https://elm-spa-example.netlify.com/"
      , picture = ""
      , description = "A Single Page Application written in Elm"
      , keywords = [ SPA, Boilerplate ]
      , authors = [ Richard_Feldman ]
      }
    , { name = "Elm's package website"
      , code = "https://github.com/elm/package.elm-lang.org"
      , url = "https://package.elm-lang.org/"
      , picture = ""
      , description = "Website for browsing packages and exploring documentation"
      , keywords = [ SPA, Boilerplate ]
      , authors = [ Evan_Czaplicki ]
      }
    , { name = "Rubik's cube"
      , code = "https://github.com/w0rm/elm-cubik"
      , url = "https://unsoundscapes.itch.io/cubik"
      , picture = "img/rubik.png"
      , description = "Rubik's cube puzzle in the Elm language"
      , keywords = [ Rubik, Animation, WebGL, Game ]
      , authors = [ Andrey_Kuzmin ]
      }

    --
    -- Elm Europe 2018 Videos
    --
    , { name = "What is Success?"
      , code = ""
      , url = "https://www.youtube.com/watch?v=uGlzRt-FYto"
      , picture = ""
      , description = ""
      , authors = [ Evan_Czaplicki ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "Making a LaTeX-to-Html parser in elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=dmDA7iziSgs"
      , picture = ""
      , description = ""
      , authors = [ James_Carlson ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "Learning elm in 3 days using the compiler"
      , code = ""
      , url = "https://www.youtube.com/watch?v=ulki9D2OKN8"
      , picture = ""
      , description = ""
      , authors = [ Christopher_Bertels ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "ElmSEQ: A conflict-free replicated data type for elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=r_QmENb-TAA"
      , picture = ""
      , description = ""
      , authors = [ Matthias_Rella ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "Let's publish nice packages"
      , code = ""
      , url = "https://www.youtube.com/watch?v=yVn7FOQuwDM"
      , picture = ""
      , description = ""
      , authors = [ Brian_Hicks ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "Infecting the visualization design process with the elm philosophy"
      , code = ""
      , url = "https://www.youtube.com/watch?v=K-yoLxnm95A"
      , picture = ""
      , description = ""
      , authors = [ Alexander_Kachkaev ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "Now you're thinking in functions"
      , code = ""
      , url = "https://www.youtube.com/watch?v=F4fuVJNnQoo"
      , picture = ""
      , description = ""
      , authors = [ Ian_Mackenzie ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "Teaching is learning"
      , code = ""
      , url = "https://www.youtube.com/watch?v=vDdd7Gnwlmc"
      , picture = ""
      , description = ""
      , authors = [ Celine_Martinet_Sanchez ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "Rendering text with WebGL"
      , code = ""
      , url = "https://www.youtube.com/watch?v=qasFxsOCfpA"
      , picture = ""
      , description = ""
      , authors = [ Andrey_Kuzmin ]
      , keywords = [ ElmEurope2018, WebGL ]
      }
    , { name = "Live coding a Mondrian generator"
      , code = ""
      , url = "https://www.youtube.com/watch?v=xAZLceCZGks"
      , picture = ""
      , description = ""
      , authors = [ Ju_Liu ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "Make Data Structures"
      , code = ""
      , url = "https://www.youtube.com/watch?v=x1FU3e0sT1I"
      , picture = ""
      , description = ""
      , authors = [ Richard_Feldman ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "Evergreen elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=4T6nZffnfzg"
      , picture = ""
      , description = ""
      , authors = [ Mario_Rogic ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "Lessons from 100k LOC elm at Futurespace"
      , code = ""
      , url = "https://www.youtube.com/watch?v=0AosqGTEa0Q"
      , picture = ""
      , description = ""
      , authors = [ Mark_Skipper, Decio_Ferreira ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "How to build a native looking date picker with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=nal1MUgZ8Tc"
      , picture = ""
      , description = ""
      , authors = [ Thibaut_Assus ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "Getting your organization onboard with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=iZH5QyCB-Hg"
      , picture = ""
      , description = ""
      , authors = [ Emma_Cunningham ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "Working with Maybe"
      , code = ""
      , url = "https://www.youtube.com/watch?v=43eM4kNbb6c"
      , picture = ""
      , description = ""
      , authors = [ Joel_Quenneville ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "When and how to use Web Components with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=tyFe9Pw6TVE"
      , picture = ""
      , description = ""
      , authors = [ Luke_Westby ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "elm-browser: Smalltalk goodness for elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=As_vGSvjz_c"
      , picture = ""
      , description = ""
      , authors = [ Martin_Janiczek ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "The immutable animator's toolkit"
      , code = ""
      , url = "https://www.youtube.com/watch?v=Nf4rElfA8SE"
      , picture = ""
      , description = ""
      , authors = [ Matthew_Griffith ]
      , keywords = [ ElmEurope2018 ]
      }
    , { name = "3.9 million lines later: Analyzing all open source elm code"
      , code = ""
      , url = "https://www.youtube.com/watch?v=HLeQ8OmdJRk"
      , picture = ""
      , description = ""
      , authors = [ Paul_Sonnentag ]
      , keywords = [ ElmEurope2018 ]
      }

    --
    -- Elm Europe 2017 Videos
    --
    , { name = "The life of a file"
      , code = ""
      , url = "https://www.youtube.com/watch?v=XpDsk374LDE"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Evan_Czaplicki ]
      }
    , { name = "Visualizing data with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=Pf1xQ76JgmQ"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Jakub_Hampl ]
      }
    , { name = "Building Reorderable UI in Elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=UiLGIQUGFQg"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Greg_Ziegan ]
      }
    , { name = "Bringing the fun to graphics programming"
      , code = ""
      , url = "https://www.youtube.com/watch?v=Z-6ETEBNlMs"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Andrey_Kuzmin ]
      }
    , { name = "Understanding style"
      , code = ""
      , url = "https://www.youtube.com/watch?v=NYb2GDWMIm0"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Matthew_Griffith ]
      }
    , { name = "Cooking with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=C3mnyJlCqMk"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Noah_Zachary_Gordon ]
      }
    , { name = "Elm-plot : the big picture"
      , code = ""
      , url = "https://www.youtube.com/watch?v=qTdXFRloYWU"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Tereza_Sokol ]
      }
    , { name = "Multiplayer games by the boatloads. Making elm-gameroom"
      , code = ""
      , url = "https://www.youtube.com/watch?v=sBCz6atTRZk"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017, Game ]
      , authors = [ Peter_Szerzo ]
      }
    , { name = "Elm from a CTO perspective"
      , code = ""
      , url = "https://www.youtube.com/watch?v=8KWVl0D00SM"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Sebastien_Creme ]
      }
    , { name = "How to unblock yourself with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=OgH3pPXXSkY"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Noah_Hall ]
      }
    , { name = "The state of Elm 2017"
      , code = ""
      , url = "https://www.youtube.com/watch?v=BAtql6ZbvpU"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Brian_Hicks ]
      }
    , { name = "Scaling Elm Apps"
      , code = ""
      , url = "https://www.youtube.com/watch?v=DoA4Txr4GUs"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Richard_Feldman ]
      }
    , { name = "Turning the elm narrative engine inside-out"
      , code = ""
      , url = "https://www.youtube.com/watch?v=4H7iH_kymig"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Jeff_Schomay ]
      }
    , { name = "How frontend microservices help us stay flexible"
      , code = ""
      , url = "https://www.youtube.com/watch?v=U_5XKPe4fZM"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Tomek_Wiszniewski ]
      }
    , { name = "Date manipulation with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=ztqEIchSDgM"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Vincent_Billey ]
      }
    , { name = "Testing your Msgs fully"
      , code = ""
      , url = "https://www.youtube.com/watch?v=baRcusTHc8E"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Martin_Janiczek ]
      }
    , { name = "Dive: building Prezi-like presentations with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=TRATeS93bsA"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Matthias_Rella ]
      }
    , { name = "Persistent collections: how they work and when to use them"
      , code = ""
      , url = "https://www.youtube.com/watch?v=mmiNobpx7eI"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Robin_Heggelund_Hansen ]
      }
    , { name = "Music chords charts in elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=c3BFNqk9jF0"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Christophe_Benz ]
      }
    , { name = "Elm from a business perspective"
      , code = ""
      , url = "https://www.youtube.com/watch?v=DvQI1KntMhk"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Amitai_Burstein ]
      }
    , { name = "Elm native UI in production"
      , code = ""
      , url = "https://www.youtube.com/watch?v=Dr3kQB8byEo"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Josh_Steiner ]
      }
    , { name = "Elmception: supercharging presentations with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=EyBtz8xZz7U"
      , picture = ""
      , description = ""
      , keywords = [ ElmEurope2017 ]
      , authors = [ Mario_Rogic ]
      }

    --
    -- Elm Conf 2018 Videos
    --
    , { name = "Immutable Relational Data"
      , code = ""
      , url = "https://www.youtube.com/watch?v=28OdemxhfbU"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2018 ]
      , authors = [ Richard_Feldman ]
      }
    , { name = "Facilitating Technology Change Through Cultural Change"
      , code = ""
      , url = "https://www.youtube.com/watch?v=5WVXCy1Q88o"
      , picture = ""
      , description = "When we discuss how to help teams adopt new technologies or paradigms like Elm of Functional Programming, we often focus on a technical glide path. Not enough attention is given to building a team culture that is open to experimenting with new technologies and adopting them if the experiments go well. This talk will explore strategies and tactics to build such a culture through the lens of my team's experiences at Ford Motor Company. Building such a culture has produced a number of pleasant side effects, including allowing our team to adopt Elm for all of our front-end development."
      , keywords = [ ElmConf2018, Ford ]
      , authors = [ Grant_Maki ]
      }
    , { name = "Structured Editing for Elm* in Elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=-TFL6E1uSHE"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2018 ]
      , authors = [ Ravi_Chugh ]
      }
    , { name = "Programming Robots with Elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=WQFwnKVLDdI"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2018 ]
      , authors = [ Anthony_Deschamps ]
      }
    , { name = "Building a Toolkit for Design"
      , code = ""
      , url = "https://www.youtube.com/watch?v=Ie-gqwSHQr0"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2018 ]
      , authors = [ Matthew_Griffith ]
      }
    , { name = "Types Without Borders"
      , code = ""
      , url = "https://www.youtube.com/watch?v=memIRXFSNkU"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2018 ]
      , authors = [ Dillon_Kearns ]
      }
    , { name = "Complex Animations Done Well"
      , code = ""
      , url = "https://www.youtube.com/watch?v=trgET9YU37M"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2018 ]
      , authors = [ Sam_Rowe ]
      }
    , { name = "Naming Things in Elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=trgET9YU37M"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2018 ]
      , authors = [ Ally_Kelly_McKnight ]
      }
    , { name = "Demystifying Parsers"
      , code = ""
      , url = "https://www.youtube.com/watch?v=M9ulswr1z0E"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2018, JSON ]
      , authors = [ Tereza_Sokol ]
      }

    --
    -- Elm Conf 2017 Videos
    --
    , { name = "Building a Slippy Map Library in Pure Elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=kWIiXbn1FXU"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2017 ]
      , authors = [ Jonas_Coch ]
      }
    , { name = "Accessibility with Elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=G_TNRuoF3FE"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2017 ]
      , authors = [ Tessa_Kelly ]
      }
    , { name = "The Importance of Ports"
      , code = ""
      , url = "https://www.youtube.com/watch?v=P3pL85n9_5s"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2017 ]
      , authors = [ Murphy_Randle ]
      }
    , { name = "Putting the Elm Platform in the Browser"
      , code = ""
      , url = "https://www.youtube.com/watch?v=gNWx-zWxUd4"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2017 ]
      , authors = [ Luke_Westby ]
      }
    , { name = "Solving the Boolean Identity Crisis"
      , code = ""
      , url = "https://www.youtube.com/watch?v=6TDKHGtAxeg"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2017 ]
      , authors = [ Jeremy_Fairbank ]
      }
    , { name = "Activate Your Interface with CSS Animations"
      , code = ""
      , url = "https://www.youtube.com/watch?v=Zje8MN9whF0"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2017, CSS ]
      , authors = [ Peter_Zingg ]
      }
    , { name = "Mogee or how we fit Elm in a 64×64 grid"
      , code = ""
      , url = "https://www.youtube.com/watch?v=NRXTMaXO15I"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2017 ]
      , authors = [ Andrey_Kuzmin ]
      }
    , { name = "If Coco Chanel Reviewed Elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=Wiw3YcwGwrU"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2017 ]
      , authors = [ Tereza_Sokol ]
      }
    , { name = "Teaching Elm to Beginners"
      , code = ""
      , url = "https://www.youtube.com/watch?v=G-GhUxeYc1U"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2017 ]
      , authors = [ Richard_Feldman ]
      }
    , { name = "Elm Conf 2017 Closing Q&A"
      , code = ""
      , url = "https://www.youtube.com/watch?v=iCvvSqCnvWY"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2017 ]
      , authors = []
      }

    --
    -- Elm Conf 2016 Videos
    --
    , { name = "Code is the Easy Part"
      , code = ""
      , url = "https://www.youtube.com/watch?v=DSjbTC-hvqQ"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2016 ]
      , authors = [ Evan_Czaplicki ]
      }
    , { name = "Beyond Hello World and Todo Lists"
      , code = ""
      , url = "https://www.youtube.com/watch?v=vpc80c5iC6k"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2016 ]
      , authors = [ Ossi_Hanhinen ]
      }
    , { name = "Compilers as Therapists, or Why Elm is Good for ADHD"
      , code = ""
      , url = "https://www.youtube.com/watch?v=wpYFTG-uViE"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2016 ]
      , authors = [ Luke_Westby ]
      }
    , { name = "Rich Animation"
      , code = ""
      , url = "https://www.youtube.com/watch?v=DsDwYqsLU3E"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2016 ]
      , authors = [ Matthew_Griffith ]
      }
    , { name = "Functional Data Structures"
      , code = ""
      , url = "https://www.youtube.com/watch?v=UBuFHCsz16A"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2016 ]
      , authors = [ Tessa_Kelly ]
      }
    , { name = "0-60 in 15 Minutes: Building a Realtime App With Elm and Horizon"
      , code = ""
      , url = "https://www.youtube.com/watch?v=fTHHn7NhU3w"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2016 ]
      , authors = [ Abadi_Kurniawaan ]
      }
    , { name = "Rolling Random Romans"
      , code = ""
      , url = "https://www.youtube.com/watch?v=YxGWQdFo2Yc"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2016 ]
      , authors = [ Joel_Quenneville ]
      }
    , { name = "Building an Interactive Storytelling Framework in Elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=t8RSxzpw1Yw"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2016 ]
      , authors = [ Jeff_Schomay ]
      }
    , { name = "The Clockwork Gardener: Growing an Elm App With Templates"
      , code = ""
      , url = "https://www.youtube.com/watch?v=jJ4e6cIBgYM&"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2016 ]
      , authors = [ Jessica_Kerr ]
      }
    , { name = "Nightingale.space - Elm and Crowd-Sourced Music Making"
      , code = ""
      , url = "https://www.youtube.com/watch?v=lPXVfqwYmEA&"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2016 ]
      , authors = [ Murphy_Randle ]
      }
    , { name = "Making Impossible States Impossible"
      , code = ""
      , url = "https://www.youtube.com/watch?v=IcgmSRJHu_8&"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2016 ]
      , authors = [ Richard_Feldman ]
      }
    , { name = "Q&A Panel"
      , code = ""
      , url = "https://www.youtube.com/watch?v=LCNs92YQjhw&s"
      , picture = ""
      , description = ""
      , keywords = [ ElmConf2016 ]
      , authors = []
      }

    --
    -- POSTS
    --
    , { name = "Demystifying Elm JSON Decoders"
      , code = "https://github.com/zwilias/elm-demystify-decoders"
      , url = "https://blog.ilias.xyz/demystifying-decoders-d294ed35bc6e"
      , picture = ""
      , description = "Figure out how JSON decoders in Elm work by writing them. Step by step."
      , keywords = [ JSON ]
      , authors = [ Ilias_Van_Peer ]
      }
    , { name = "The state of Elm 2016"
      , code = ""
      , url = "https://www.brianthicks.com/post/2016/04/22/state-of-elm-2016-results/"
      , picture = ""
      , description = "The \"State of Elm\" survey takes the pulse of the Elm community."
      , keywords = []
      , authors = [ Brian_Hicks ]
      }
    , { name = "The state of Elm 2017"
      , code = ""
      , url = "https://www.brianthicks.com/post/2017/07/27/state-of-elm-2017-results/"
      , picture = ""
      , description = "The \"State of Elm\" survey takes the pulse of the Elm community."
      , keywords = []
      , authors = [ Brian_Hicks ]
      }
    , { name = "The state of Elm 2018"
      , code = ""
      , url = "https://www.brianthicks.com/post/2018/12/26/state-of-elm-2018-results/"
      , picture = ""
      , description = "The \"State of Elm\" survey takes the pulse of the Elm community."
      , keywords = []
      , authors = [ Brian_Hicks ]
      }
    , { name = "Ellie, The Elm Live Editor"
      , code = "https://github.com/ellie-app/ellie"
      , url = "https://ellie-app.com/"
      , picture = "img/ellie.png"
      , description = "Ellie is a web application with an Elixir backend and an Elm frontend. It runs inside Docker."
      , keywords = [ Docker, Elixir ]
      , authors = [ Luke_Westby ]
      }
    , { name = "Asteroids"
      , code = "https://github.com/justinmimbs/asteroids"
      , url = "https://justinmimbs.github.io/asteroids/"
      , picture = "img/asteroids.png"
      , description = "An Asteroids clone made with Elm"
      , keywords = [ Game ]
      , authors = [ Justin_Mimbs ]
      }
    , { name = "Font builder"
      , code = "https://github.com/justgook/FontBuilder2"
      , url = "https://justgook.github.io/FontBuilder2/"
      , picture = "img/font-builder.png"
      , description = "To convert any google font to image and mapping file"
      , keywords = [ Font ]
      , authors = [ Roman_Potashow ]
      }

    -- Oslo Conf 2017
    , { authors = [ Brian_Hicks ]
      , name = "The State of Elm 2017, Extended Edition"
      , keywords = [ OsloElmDay2017 ]
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=NKl0dtSe8rs&t=5"
      , description = ""
      }
    , { authors = [ David_Ed_Mellum ]
      , name = "Rewriting a Production SPA in Elm"
      , keywords = [ OsloElmDay2017 ]
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=EQj0RZsCLKg&t=0"
      , description = ""
      }
    , { authors = [ Magnus_Rundberget ]
      , name = "Elm Editor Support - Perspectives From an Editor Hacker"
      , keywords = [ OsloElmDay2017 ]
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=ExGkdmey0n4&t=0"
      , description = ""
      }
    , { authors = [ Noah_Hall ]
      , name = "Everything in Elm"
      , keywords = [ OsloElmDay2017 ]
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=FUHlnxQJTMM&t=0"
      , description = ""
      }
    , { authors = [ Felix_Lamouroux ]
      , name = "Localization for Elm as a pre-build phase"
      , keywords = [ OsloElmDay2017 ]
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=RcHV6R-Jq00&t=0"
      , description = ""
      }
    , { authors = [ Austin_Bingham ]
      , name = "An Elm Kernel for Jupyter"
      , keywords = [ OsloElmDay2017 ]
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=p8cHarclNnc&t=0"
      , description = "Introduction of the Elm kernel for Jupyter notebooks. Jupyter (a.k.a. IPython) notebooks are a way to interleave text and executable code, and this new kernel allows you to embed Elm code in them. "
      }
    , { authors = [ Luke_Westby ]
      , name = "Putting the Elm Platform in the Browser"
      , keywords = [ OsloElmDay2017 ]
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=GwmVELtQnOI&t=0"
      , description = ""
      }
    , { authors = [ Erlend_Hamberg ]
      , name = "Bootstrapping a project with Elm"
      , keywords = [ OsloElmDay2017 ]
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=6ryDY5thUCg&t=0"
      , description = ""
      }
    , { authors = [ Nils_Eriksson ]
      , name = "Move Fast and Don’t Break Things. Running a Startup on Elm"
      , keywords = [ OsloElmDay2017 ]
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=7XqZ6JUi6_E&t=0s"
      , description = ""
      }
    , { authors = [ Richard_Feldman ]
      , name = "The Design Evolution of elm-css and elm-test"
      , keywords = [ OsloElmDay2017, CSS ]
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=n5faeSW71ko&t=50s"
      , description = ""
      }
    , { authors = [ Michael_Bylstra ]
      , name = "HTML to Elm"
      , keywords = [ HTML, Converter ]
      , picture = ""
      , code = "https://github.com/mbylstra/html-to-elm"
      , url = "https://mbylstra.github.io/html-to-elm/"
      , description = ""
      }
    , { authors = [ Matthew_Buscemi ]
      , name = "The Five Stages of JSON Decoding in Elm"
      , keywords = [ JSON ]
      , picture = ""
      , code = ""
      , url = "https://medium.com/@matthew.buscemi/the-five-stages-of-json-decoding-in-elm-e695adb9162a"
      , description = ""
      }
    , { authors = [ Remi_Lefevre ]
      , name = "elm-doc-preview"
      , keywords = []
      , picture = "img/elm-doc-preview.png"
      , code = "https://github.com/dmy/elm-doc-preview"
      , url = "https://www.npmjs.com/package/elm-doc-preview"
      , description = "This is a documentation previewer for Elm packages (>= 0.19). It allows previewing README.md and docs.json files (generated with elm make --docs=docs.json). It aims at rendering documentation exactly like the official package website."
      }
    , { authors = [ Remi_Lefevre ]
      , name = "On-line Documentation Previewer"
      , keywords = []
      , picture = "img/elm-doc-preview.png"
      , code = ""
      , url = "https://elm-doc-preview.netlify.com/"
      , description = "This is a documentation previewer for Elm packages (>= 0.19). It allows previewing README.md and docs.json files (generated with elm make --docs=docs.json). It aims at rendering documentation exactly like the official package website."
      }
    , { authors = [ Mats_Stijlaart ]
      , name = "Elm Analyse"
      , keywords = []
      , picture = "img/elm-analyse.png"
      , code = "https://github.com/stil4m/elm-analyse"
      , url = "https://www.npmjs.com/package/elm-analyse"
      , description = "A tool that allows you to analyse your Elm code, identify deficiencies and apply best practices."
      }
    , { authors = []
      , name = "Elm Weekly"
      , keywords = []
      , picture = "img/elm-weekly.png"
      , code = ""
      , url = "http://www.elmweekly.nl/archive"
      , description = "The wonderful world of the Elm programming language, in your inbox."
      }
    , { authors = [ Jeremy_Fairbank ]
      , name = "Toward a Better Front End Architecture: Elm - Codemash 2017"
      , keywords = []
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=EDp6UmaA9CM"
      , description = "Amidst the overwhelming cacophony of competing JavaScript frameworks, Elm is a promising voice."
      }
    , { authors = []
      , name = "Coding the Art, Continued"
      , keywords = [ Animation, WebGL ]
      , picture = "img/code2art.png"
      , code = "https://github.com/Jetbrains/elmsfeuer"
      , url = "https://www.jetbrains.com/goodies/code2art/#tron"
      , description = "Crafting tools and making them better is human nature. JetBrains has built its business on this tradition, by creating tools which developers love using daily and which automate the routine parts of their jobs."
      }
    , { authors = []
      , name = "Built in Elm"
      , keywords = []
      , picture = ""
      , code = "https://github.com/elm-community/builtwithelm"
      , url = "http://builtwithelm.co/"
      , description = "A list of projects and apps built with Elm."
      }
    , { authors = [ Francesco_Orsenigo ]
      , name = "Herzog Drei"
      , keywords = [ Game ]
      , picture = "img/herzog-drei.png"
      , code = "https://github.com/xarvh/herzog-drei/"
      , url = "https://xarvh.github.io/herzog-drei/"
      , description = ""
      }
    , { authors = [ Halohalospecial ]
      , name = "Elmjutsu"
      , keywords = [ Atom, IDEs ]
      , picture = ""
      , code = "https://github.com/halohalospecial/atom-elmjutsu"
      , url = "https://atom.io/packages/elmjutsu"
      , description = "A bag of tricks for developing with Elm. (Atom package)"
      }
    , { authors = [ Matthew_Buscemi ]
      , name = "Elm Lens"
      , keywords = [ Atom, IDEs ]
      , picture = ""
      , code = "https://github.com/mbuscemi/elm-lens"
      , url = "https://atom.io/packages/elm-lens"
      , description = "Elm code visualizations for maximum productivity"
      }
    , { authors = [ Richard_Feldman ]
      , name = "Elm in Action"
      , keywords = [ Book ]
      , picture = "img/book-elm-in-action.png"
      , code = ""
      , url = "https://www.manning.com/books/elm-in-action"
      , description = ""
      }
    , { authors = [ Matthew_Griffith ]
      , name = "Why Elm?"
      , keywords = [ Book ]
      , picture = "img/book-why-elm.png"
      , code = ""
      , url = "https://www.oreilly.com/web-platform/free/files/why-elm.pdf"
      , description = ""
      }
    , { authors = [ Jeremy_Fairbank ]
      , name = "Programming Elm"
      , keywords = [ Book ]
      , picture = "img/book-programming-elm.png"
      , code = ""
      , url = "https://pragprog.com/book/jfelm/programming-elm"
      , description = ""
      }
    , { authors = [ Ossi_Hanhinen, Matias_Klemola ]
      , name = "elm-shared-state"
      , keywords = [ SPA, Boilerplate ]
      , picture = "img/tako.png"
      , code = "https://github.com/ohanhi/elm-shared-state"
      , url = "https://ohanhi.github.io/elm-shared-state/"
      , description = "Example app on managing shared state in large Elm SPAs. (ex elm-taco)"
      }
    , { authors = [ Evan_Czaplicki ]
      , name = "The Hard Parts of Open Source"
      , keywords = []
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=o_4EX4dPppA"
      , description = "As more people enter /r/elm and the Elm discourse, I have thought a lot about how \"online communities\" work. Patterns of conflict. Why those patterns exist. Structures that would diffuse that conflict in healthy ways."
      }
    , { authors = [ Evan_Czaplicki ]
      , name = "Lets be mainstream!"
      , keywords = []
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=oYk8CKH7OhE"
      , description = "User-focused design in Elm"
      }
    , { authors = [ Evan_Czaplicki ]
      , name = "Convergent Evolution"
      , keywords = []
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=jl1tGiUiTtI"
      , description = ""
      }
    , { authors = [ Evan_Czaplicki ]
      , name = "Functional All the Things: A Functional Panel"
      , keywords = []
      , picture = ""
      , code = ""
      , url = "https://www.youtube.com/watch?v=re4oOOkqs3E"
      , description = ""
      }
    , { authors = [ Roman_Frolow ]
      , name = "Awesome Elm Gamedev"
      , keywords = [ Game, ListOfLinks ]
      , picture = ""
      , code = ""
      , url = "https://github.com/rofrol/awesome-elm-gamedev"
      , description = "A curated list of useful Elm tutorials, libraries and software for Game Development. Inspired by awesome list. Feel free to contribute."
      }
    , { authors = [ Rogerio_Chaves ]
      , name = "Spades"
      , keywords = [ SPA, Boilerplate ]
      , picture = ""
      , code = ""
      , url = "https://github.com/rogeriochaves/spades"
      , description = "Spades is a framework for Elm that helps you quickly start a Single Page Application (SPA) ready to the real world"
      }
    , { authors = [ Sosuke ]
      , name = "ELMineSweeper"
      , keywords = [ Game, ElmUI_Example ]
      , picture = ""
      , code = "https://github.com/nikueater/elminesweeper"
      , url = "https://dazzling-turing-db7f8b.netlify.com/"
      , description = "Minesweeper written in Elm"
      }
    , { authors = [ Dillon_Kearns ]
      , name = "Incremental Elm"
      , keywords = [ ElmUI_Example, SPA ]
      , picture = ""
      , code = "https://github.com/IncrementalElm/incremental-elm-web"
      , url = "http://incrementalelm.com/"
      , description = "Incremental Elm Consulting Web site"
      }
    , { authors = [ Chandu_Tennety ]
      , name = "Conway's Game of Life"
      , keywords = [ ElmUI_Example, Game ]
      , picture = ""
      , code = "https://github.com/tennety/elm-game-of-life"
      , url = "https://tennety.github.io/elm-game-of-life/"
      , description = "A little implementation of Conway's Game of Life in Elm"
      }
    , { authors = [ Ronan ]
      , name = "Roteiro"
      , keywords = [ ElmUI_Example ]
      , picture = ""
      , code = "https://github.com/ronanyeah/roteiro"
      , url = "https://roteiro.training/"
      , description = ""
      }
    , { authors = [ Fabian_Kirchner ]
      , name = "Grid"
      , keywords = [ ElmUI_Example, Game ]
      , picture = ""
      , code = "https://github.com/kirchner/grid"
      , url = "https://kirchner.github.io/grid/"
      , description = ""
      }
    , { authors = [ Jo_Wood ]
      , name = "The Turing Machine"
      , keywords = [ ElmUI_Example, Game ]
      , picture = ""
      , code = "https://github.com/jwoLondon/turingmachine"
      , url = "https://www.staff.city.ac.uk/~jwo/tutorials/tm/"
      , description = "Web-based Turing Machine for teaching and learning"
      }
    , { authors = [ Rogerio_Chaves ]
      , name = "Rubber"
      , keywords = [ Mathematics ]
      , picture = ""
      , code = "https://github.com/rogeriochaves/rubber/"
      , url = "https://rogeriochaves.github.io/rubber/"
      , description = "Evaluate LaTeX math code"
      }
    , { authors = [ Remi_Van_Keisbelck ]
      , name = "IBM Decision Composer"
      , keywords = [ IBM ]
      , picture = ""
      , code = "https://discourse.elm-lang.org/t/ibm-releases-elm-powered-app/2364"
      , url = "https://decision-composer.ibm.com/"
      , description = "A tool that allows business users to model and implement complex Decisions via graphical tools and human-readable artifacts instead of code."
      }
    , { authors = [ Yosuke_Torii ]
      , name = "基礎からわかる Elm"
      , keywords = [ Book ]
      , picture = "img/book-elm-japanese.jpg"
      , code = ""
      , url = "https://www.amazon.co.jp/dp/4863542224/ref=asc_df_48635422242566179/?tag=jpgo-22&creative=9303&creativeASIN=4863542224&linkCode=df0&hvadid=295706574430&hvpos=1o1&hvnetw=g&hvrand=560766736247070493&hvpone=&hvptwo=&hvqmt=&hvdev=c&hvdvcmdl=&hvlocint=&hvlocphy=1009300&hvtargid=pla-526453139716&th=1&psc=1"
      , description = ""
      }
    , { authors = [ Justin_Herrick ]
      , name = "elm-companies"
      , keywords = [ ListOfLinks ]
      , picture = ""
      , code = ""
      , url = "https://github.com/jah2488/elm-companies"
      , description = "A list of companies using Elm in production"
      }
    , { authors = []
      , name = "Elm support for older browsers? (IE 9, 10, …)"
      , keywords = [ IE ]
      , picture = ""
      , code = ""
      , url = "https://discourse.elm-lang.org/t/elm-support-for-older-browsers-ie-9-10/744/8"
      , description = ""
      }
    , { authors = [ Martin_Feineis ]
      , name = "An Elm to the Past «Bringing Elm 0.18 to IE8+»"
      , keywords = [ IE ]
      , picture = ""
      , code = ""
      , url = "http://canena.de/blog/2018-02-24-an-elm-to-the-past/"
      , description = ""
      }
    , { authors = []
      , name = "test-elm-performance"
      , keywords = [ ElmUI_Example, Performance ]
      , picture = ""
      , code = ""
      , url = "https://github.com/webbhuset/test-elm-performance"
      , description = "A simple, non-scientific test of the render performance of the Elm Virtual DOM"
      }
    , { authors = [ Miyamoen ]
      , name = "Bibliopola"
      , keywords = [ ElmUI_Example ]
      , picture = ""
      , code = "https://github.com/miyamoen/bibliopola"
      , url = "https://miyamoen.github.io/bibliopola/"
      , description = "UI Catalog for Elm applications built by elm-ui inspired by Storybook"
      }
    , { authors = [ Tomas_Latal ]
      , name = "Elm 0.19 readiness helper"
      , keywords = [ ElmUI_Example, DevTool ]
      , picture = ""
      , code = ""
      , url = "https://kraklin.github.io/elm-readiness/"
      , description = "Keep track of transition state of your dependencies in your own Elm app to Elm 0.19"
      }
    , { authors = [ Jan_Hrcek ]
      , name = "Surjective / Injective"
      , keywords = [ ElmUI_Example, Mathematics ]
      , picture = ""
      , code = "https://github.com/jhrcek/surjective-injective"
      , url = "https://janhrcek.cz/surjective-injective/"
      , description = "Visual exploration of surjectivity and injectivity (as a properties of mathematical functions)"
      }
    , { authors = [ Keith_Lazuka ]
      , name = "intellij-elm"
      , keywords = [ Intellij, IDEs ]
      , picture = ""
      , code = "https://github.com/klazuka/intellij-elm"
      , url = "https://klazuka.github.io/intellij-elm/"
      , description = "Elm language support for IntelliJ, WebStorm, PhpStorm and PyCharm (JetBrains)"
      }
    , { authors = [ Suresh_Yadali ]
      , name = "MHC Truck Search"
      , keywords = [ ElmUI_Example ]
      , picture = ""
      , code = "https://github.com/yadalis/mhctrucksearch"
      , url = "https://mhctrucksmartsearch.azurewebsites.net/"
      , description = "A super fast search/fitler trucks to give the results instantly"
      }
    , { authors = [ Markus_Laire ]
      , name = "elm-dependencies-analyzer"
      , keywords = [ DevTool ]
      , picture = ""
      , code = "https://github.com/malaire/elm-dependencies-analyzer"
      , url = "https://www.markuslaire.com/github/elm-dependencies-analyzer/"
      , description = "A little program I created for analyzing package dependencies of application elm.json"
      }
    ]
