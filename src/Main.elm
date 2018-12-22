module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



-- MODEL


type alias Model =
    { width : Int
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { width = 0 }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnResize x y ->
            ( { model | width = x }, Cmd.none )



-- VIEW


buttonAttributes : List (Attr () msg)
buttonAttributes =
    [ Background.color <| rgb 0.8 0.6 1
    , Border.rounded 20
    , padding 10
    , width <| px 100
    , Font.center
    ]


empty : Element msg
empty =
    el
        [ width <| px 100
        , height <| px 100
        , Border.rounded 25
        , clip
        , Background.color <| rgb 0.8 0.8 0.8
        ]
        none


view : Model -> Html Msg
view model =
    layout [ width fill, height fill ] <|
        column
            [ padding 50
            , spacing 30
            ]
            [ paragraph [ Font.size 24 ] [ text "Elm Bookmarks" ]
            , wrappedRow
                [ Font.size 16
                , spacing 10
                ]
                (List.map
                    (\item ->
                        link []
                            { url = item.url
                            , label =
                                column [ spacing 10 ]
                                    [ el [ width <| px 100, clip ] <| text item.name
                                    , if item.picture == "" then
                                        empty

                                      else
                                        image
                                            [ width <| px 100
                                            , height <| px 100
                                            , Border.rounded 25
                                            , clip
                                            ]
                                            { src = item.picture, description = item.name }
                                    ]
                            }
                    )
                    bookmarks
                )
            , paragraph [ Font.size 24 ] [ text "People" ]
            , wrappedRow
                [ Font.size 16
                , spacing 10
                ]
                (List.map
                    (\item ->
                        link []
                            { url = ""
                            , label =
                                column [ spacing 10 ]
                                    [ el [ width <| px 100, clip ] <| text <| item.name
                                    , if item.picture == "" then
                                        empty

                                      else
                                        image
                                            [ width <| px 100
                                            , height <| px 100
                                            , Border.rounded 25
                                            , clip
                                            ]
                                            { src = item.picture, description = item.name }
                                    ]
                            }
                    )
                    people
                )
            , paragraph [ Font.size 24 ] [ text "Keywords" ]
            , wrappedRow
                [ Font.size 16
                , spacing 10
                ]
                (List.map
                    (\item ->
                        link []
                            { url = ""
                            , label =
                                column [ spacing 10 ]
                                    [ el [ width <| px 100, clip ] <| text <| item.name
                                    , if item.picture == "" then
                                        empty

                                      else
                                        el
                                            [ width <| px 100
                                            , height <| px 100
                                            , Border.rounded 25
                                            , clip
                                            , Background.color <| rgb 0.8 0.8 0.8
                                            ]
                                        <|
                                            image
                                                [ width <| px 70
                                                , height <| px 70
                                                , centerX
                                                , centerY
                                                ]
                                                { src = item.picture, description = item.name }
                                    ]
                            }
                    )
                    keywords
                )
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize OnResize



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Bookmark =
    { code : String
    , url : String
    , picture : String
    , description : String
    , keywords : List Keywords
    , name : String
    , authors : List Person
    }


bookmarks : List Bookmark
bookmarks =
    [ { name = "Moltiplication Table"
      , code = "https://github.com/HappMacDonald/MultiplicationTable"
      , url = "http://lightsecond.com/MultiplicationTable/"
      , picture = ""
      , description = ""
      , keywords = [ Game, Mathematics ]
      , authors = []
      }
    , { name = "Oslo Elm Day 2017"
      , code = ""
      , url = "https://2017.osloelmday.no/"
      , picture = "img/oslo-elm-day-2017.png"
      , description = ""
      , keywords = [ Conference, OsloElmDay, OsloElmDay2017 ]
      , authors = []
      }
    , { name = "Oslo Elm Day 2019"
      , code = ""
      , url = "https://osloelmday.no/"
      , picture = "img/oslo-elm-day-2019.png"
      , description = ""
      , keywords = [ Conference, OsloElmDay, OsloElmDay2019 ]
      , authors = []
      }
    , { name = "elm-conf 2017"
      , code = ""
      , url = "https://2017.elm-conf.us/"
      , picture = "img/elm-conf-2017.png"
      , description = ""
      , keywords = [ Conference, ElmConf, ElmConf2017 ]
      , authors = [ Brian_Hicks ]
      }
    , { name = "elm-conf 2018"
      , code = ""
      , url = "https://2018.elm-conf.us/"
      , picture = "img/elm-conf-2018.png"
      , description = ""
      , keywords = [ Conference, ElmConf, ElmConf2018 ]
      , authors = [ Brian_Hicks ]
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
      , picture = ""
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
    , { name = "Tangram"
      , code = "https://github.com/lucamug/elm-tangram"
      , url = "http://elm-tangram.surge.sh/"
      , picture = ""
      , description = "Draggable Tangram in Elm"
      , keywords = [ Animation, Tangram, Game ]
      , authors = [ Luca_Mugnaini ]
      }
    , { name = "Elm SPA Example"
      , code = "https://github.com/rtfeldman/elm-spa-example"
      , url = "https://elm-spa-example.netlify.com/"
      , picture = ""
      , description = "A Single Page Application written in Elm"
      , keywords = [ SPA, Boilerplate ]
      , authors = [ Richard_Feldman ]
      }
    , { name = "Rubik's cube"
      , code = "https://github.com/w0rm/elm-cubik"
      , url = "https://unsoundscapes.itch.io/cubik"
      , picture = "img/rubik.png"
      , description = "Rubik's cube puzzle in the Elm language"
      , keywords = [ Rubik, Animation, WebGl ]
      , authors = [ Unsoundscapes ]
      }
    , { name = "Elm Europe 2018"
      , code = ""
      , url = "https://2018.elmeurope.org/"
      , picture = ""
      , description = "Two-day conference taking place in Villejuif, near Paris on July, 5-6th 2018"
      , keywords = [ Conference ]
      , authors = []
      }
    , { name = "Oslo Elm Day 2017 - Videos"
      , code = ""
      , url = "https://www.youtube.com/playlist?list=PLcAzxXzXQlPZsNcYycHittqeF3UG4dGli"
      , picture = ""
      , description = "Elm conference in Oslo, Norway"
      , keywords = [ Conference, OsloElmDay2017, Talk ]
      , authors = []
      }
    , { name = "Elm Europe 2017 - Talks Videos"
      , code = ""
      , url = "https://www.youtube.com/playlist?list=PL-cYi7I913S8cGyZWdN6YVZ028iS9BfpM"
      , picture = ""
      , description = ""
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = []
      }

    --
    --
    , { name = "The life of a file"
      , code = ""
      , url = "https://www.youtube.com/watch?v=XpDsk374LDE"
      , picture = ""
      , description = "#1 47:01"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Evan_Czaplicki ]
      }
    , { name = "Visualizing data with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=Pf1xQ76JgmQ"
      , picture = ""
      , description = "#2 23:00"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Jakub_Hampl ]
      }
    , { name = "Building Reorderable UI in Elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=UiLGIQUGFQg"
      , picture = ""
      , description = "#3 21:06"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Greg_Ziegan ]
      }
    , { name = "Bringing the fun to graphics programming"
      , code = ""
      , url = "https://www.youtube.com/watch?v=Z-6ETEBNlMs"
      , picture = ""
      , description = "#4 23:03"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Andrey_Kuzmin ]
      }
    , { name = "Understanding style"
      , code = ""
      , url = "https://www.youtube.com/watch?v=NYb2GDWMIm0"
      , picture = ""
      , description = "#5 21:21"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Matthew_Griffith ]
      }
    , { name = "Cooking with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=C3mnyJlCqMk"
      , picture = ""
      , description = "#6 23:16"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Noah_Zachary_Gordon ]
      }
    , { name = "Elm-plot : the big picture"
      , code = ""
      , url = "https://www.youtube.com/watch?v=qTdXFRloYWU"
      , picture = ""
      , description = "#7 18:34"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Tereza_Sokol ]
      }
    , { name = "Multiplayer games by the boatloads. Making elm-gameroom"
      , code = ""
      , url = "https://www.youtube.com/watch?v=sBCz6atTRZk"
      , picture = "img/multiplayer_games_by_the_boatloads.png"
      , description = "#8 24:16"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Peter_Szerzo ]
      }
    , { name = "Elm from a CTO perspective"
      , code = ""
      , url = "https://www.youtube.com/watch?v=8KWVl0D00SM"
      , picture = ""
      , description = "#9 39:18"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Sebastien_Creme ]
      }
    , { name = "How to unblock yourself with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=OgH3pPXXSkY"
      , picture = ""
      , description = "#10 16:39"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Noah_Hall ]
      }
    , { name = "The State of Elm 2017"
      , code = ""
      , url = "https://www.youtube.com/watch?v=BAtql6ZbvpU"
      , picture = ""
      , description = "#11 30:44"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Brian_Hicks ]
      }
    , { name = "Scaling Elm Apps"
      , code = ""
      , url = "https://www.youtube.com/watch?v=DoA4Txr4GUs"
      , picture = ""
      , description = "#12 59:51"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Richard_Feldman ]
      }
    , { name = "Turning the elm narrative engine inside-out"
      , code = ""
      , url = "https://www.youtube.com/watch?v=4H7iH_kymig"
      , picture = ""
      , description = "#13 27:43"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Jeff_Schomay ]
      }
    , { name = "How frontend microservices help us stay flexible"
      , code = ""
      , url = "https://www.youtube.com/watch?v=U_5XKPe4fZM"
      , picture = ""
      , description = "#14 26:40"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Tomek_Wiszniewski ]
      }
    , { name = "Date manipulation with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=ztqEIchSDgM"
      , picture = ""
      , description = "#15 20:28"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Vincent_Billey ]
      }
    , { name = "Testing your Msgs fully"
      , code = ""
      , url = "https://www.youtube.com/watch?v=baRcusTHc8E"
      , picture = ""
      , description = "#16 21:17"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Martin_Janiczek ]
      }
    , { name = "Dive: building Prezi-like presentations with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=TRATeS93bsA"
      , picture = ""
      , description = "#17 23:00"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Matthias_Rella ]
      }
    , { name = "Persistent collections: how they work and when to use them"
      , code = ""
      , url = "https://www.youtube.com/watch?v=mmiNobpx7eI"
      , picture = ""
      , description = "#18 24:02"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Robin_Heggelund_Hansen ]
      }
    , { name = "Music chords charts in elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=c3BFNqk9jF0"
      , picture = ""
      , description = "#19 31:38"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Christophe_Benz ]
      }
    , { name = "Elm from a business perspective"
      , code = ""
      , url = "https://www.youtube.com/watch?v=DvQI1KntMhk"
      , picture = ""
      , description = "#20 25:11"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Amitai_Burstein ]
      }
    , { name = "Elm native UI in production"
      , code = ""
      , url = "https://www.youtube.com/watch?v=Dr3kQB8byEo"
      , picture = ""
      , description = "#21 21:18"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Josh_Steiner ]
      }
    , { name = "Elmception: supercharging presentations with elm"
      , code = ""
      , url = "https://www.youtube.com/watch?v=EyBtz8xZz7U"
      , picture = ""
      , description = "#22 29:08"
      , keywords = [ Conference, ElmEurope2017, Talk ]
      , authors = [ Mario_Rogic ]
      }

    {-
       , { name = ""
         , code = ""
         , url = ""
         , picture = ""
         , description = ""
         , keywords = []
         , authors = []
         }
    -}
    ]


type Keywords
    = Game
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
    | WebGl
    | Video
    | Talk
    | ElmEurope
    | ElmEurope2017
    | ElmEurope2018
    | OsloElmDay
    | OsloElmDay2017
    | OsloElmDay2019
    | ElmConf
    | ElmConf2017
    | ElmConf2018


keywords :
    List
        { keyword : Keywords
        , name : String
        , picture : String
        }
keywords =
    [ { keyword = Game
      , name = "Game"
      , picture = "svg/joystick.svg"
      }
    , { keyword = Mathematics
      , name = "Mathematics"
      , picture = "svg/calculator.svg"
      }
    , { keyword = Conference
      , name = "Conference"
      , picture = "svg/presentation.svg"
      }
    , { keyword = UI
      , name = "UI"
      , picture = ""
      }
    , { keyword = Particles
      , name = "Particles"
      , picture = ""
      }
    , { keyword = Water
      , name = "Water"
      , picture = "svg/water.svg"
      }
    , { keyword = Confetti
      , name = "Confetti"
      , picture = "svg/confetti.svg"
      }
    , { keyword = Firework
      , name = "Firework"
      , picture = "svg/fireworks.svg"
      }
    , { keyword = Animation
      , name = "Animation"
      , picture = ""
      }
    , { keyword = Graphs
      , name = "Graphs"
      , picture = ""
      }
    , { keyword = Tangram
      , name = "Tangram"
      , picture = "svg/tangram.svg"
      }
    , { keyword = Elm_ui
      , name = "Elm_ui"
      , picture = ""
      }
    , { keyword = SPA
      , name = "SPA"
      , picture = ""
      }
    , { keyword = Boilerplate
      , name = "Boilerplate"
      , picture = ""
      }
    , { keyword = Rubik
      , name = "Rubik"
      , picture = "svg/rubik.svg"
      }
    , { keyword = WebGl
      , name = "WebGl"
      , picture = ""
      }
    ]


type Person
    = Brian_Hicks
    | Luca_Mugnaini
    | Richard_Feldman
    | Evan_Czaplicki
    | Unsoundscapes
    | Jakub_Hampl
    | Greg_Ziegan
    | Andrey_Kuzmin
    | Matthew_Griffith
    | Noah_Zachary_Gordon
    | Tereza_Sokol
    | Peter_Szerzo
    | Sebastien_Creme
    | Noah_Hall
    | Jeff_Schomay
    | Tomek_Wiszniewski
    | Vincent_Billey
    | Martin_Janiczek
    | Matthias_Rella
    | Robin_Heggelund_Hansen
    | Christophe_Benz
    | Amitai_Burstein
    | Josh_Steiner
    | Mario_Rogic


people :
    List
        { person : Person
        , name : String
        , picture : String
        , twitter : String
        , github : String
        }
people =
    [ { person = Brian_Hicks
      , name = "Brian Hicks"
      , twitter = "@brianhicks"
      , picture = "https://pbs.twimg.com/profile_images/809070189316706304/qbkqPM0v_400x400.jpg"
      , github = "https://github.com/BrianHicks"
      }
    , { person = Luca_Mugnaini
      , name = "Luca Mugnaini"
      , twitter = "@luca_mug"
      , picture = "https://osloelmday.no/images/luca-mugnaini.jpg"
      , github = "https://github.com/lucamug"
      }
    , { person = Richard_Feldman
      , name = "Richard Feldman"
      , twitter = "@rtfeldman"
      , picture = "https://pbs.twimg.com/profile_images/635812303342956545/Fo4RyEgH_400x400.jpg"
      , github = "https://github.com/rtfeldman"
      }
    , { person = Evan_Czaplicki
      , name = "Evan Czaplicki"
      , twitter = "@czaplic"
      , picture = "https://pbs.twimg.com/profile_images/443794371586977792/NxKUNpOQ_400x400.jpeg"
      , github = "https://github.com/evancz"
      }
    , { person = Jakub_Hampl
      , name = "Jakub Hampl"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Greg_Ziegan
      , name = "Greg_Ziegan"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Andrey_Kuzmin
      , name = "Andrey Kuzmin"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Matthew_Griffith
      , name = "Matthew Griffith"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Noah_Zachary_Gordon
      , name = "Noah Zachary Gordon"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Tereza_Sokol
      , name = "Tereza Sokol"
      , twitter = "@terezk_a"
      , picture = "https://cdn-images-1.medium.com/max/512/1*7O-iyPaPP0EviBCDTrw1SQ.png"
      , github = "https://github.com/terezka"
      }
    , { person = Peter_Szerzo
      , name = "Peter Szerzo"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Sebastien_Creme
      , name = "Sébastien Crème"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Noah_Hall
      , name = "Noah Hall"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Brian_Hicks
      , name = "Brian Hicks"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Richard_Feldman
      , name = "Richard Feldman"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Jeff_Schomay
      , name = "Jeff Schomay"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Tomek_Wiszniewski
      , name = "Tomek Wiszniewski"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Vincent_Billey
      , name = "Vincent Billey"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Martin_Janiczek
      , name = "Martin Janiczek"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Matthias_Rella
      , name = "Matthias Rella"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Robin_Heggelund_Hansen
      , name = "Robin Heggelund Hansen"
      , twitter = "@robheghan"
      , picture = "https://avatars0.githubusercontent.com/u/854889?s=460&v=4"
      , github = "https://github.com/Skinney"
      }
    , { person = Christophe_Benz
      , name = "Christophe Benz"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Amitai_Burstein
      , name = "Amitai Burstein"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Josh_Steiner
      , name = "Josh Steiner"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Mario_Rogic
      , name = "Mario Rogic"
      , twitter = ""
      , picture = ""
      , github = ""
      }
    , { person = Unsoundscapes
      , name = "Andrey Kuzmin"
      , twitter = "@unsoundscapes"
      , picture = "https://pbs.twimg.com/profile_images/750061928442126336/58aDIVW3_400x400.jpg"
      , github = "https://github.com/w0rm"
      }

    {-
       , { person =
         , name = ""
         , twitter = ""
         , picture = ""
         , github = ""
         }
    -}
    ]
