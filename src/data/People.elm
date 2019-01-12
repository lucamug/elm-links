module Data.People exposing
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
    , picture : String -- Picture that represent the person
    , twitter : String -- Twitter handle
    , github : String -- Github handle
    , url : String -- Link to a personal homepage
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
    , twitter = ""
    , github = ""
    , url = ""
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
    | Brian_Hicks
    | Luca_Mugnaini
    | Richard_Feldman
    | Evan_Czaplicki
    | Andrey_Kuzmin
    | Jakub_Hampl
    | Greg_Ziegan
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
    | Peter_Zingg
    | Jeremy_Fairbank
    | Luke_Westby
    | Murphy_Randle
    | Grant_Maki
    | Ravi_Chugh
    | Anthony_Deschamps
    | Dillon_Kearns
    | Sam_Rowe
    | Ally_Kelly_McKnight
    | Jonas_Coch
    | Tessa_Kelly
    | Jessica_Kerr
    | Joel_Quenneville
    | Ossi_Hanhinen
    | Abadi_Kurniawaan
    | Jesse_Thompson
    | Yosuke_Torii
    | Paul_Sonnentag
    | Emma_Cunningham
    | Thibaut_Assus
    | Mark_Skipper
    | Decio_Ferreira
    | Ju_Liu
    | Celine_Martinet_Sanchez
    | James_Carlson
    | Christopher_Bertels
    | Alexander_Kachkaev
    | Ian_Mackenzie
    | Ilias_Van_Peer
    | Justin_Mimbs
    | Roman_Potashow
    | David_Ed_Mellum
    | Magnus_Rundberget
    | Felix_Lamouroux
    | Nils_Eriksson
    | Erlend_Hamberg
    | Austin_Bingham
    | Michael_Bylstra
    | Matthew_Buscemi
    | Andreas_Hultgren
    | Jane_G
    | Keith_Lazuka
    | Remi_Lefevre
    | Mats_Stijlaart
    | Francesco_Orsenigo
    | Halohalospecial
    | Matias_Klemola
    | Roman_Frolow
    | Rogerio_Chaves
    | Sosuke
    | Chandu_Tennety
    | Ronan
    | Fabian_Kirchner
    | Jo_Wood
    | Remi_Van_Keisbelck
    | Justin_Herrick
    | Martin_Feineis
    | Miyamoen
    | Tomas_Latal
    | Jan_Hrcek
    | Suresh_Yadali
    | Markus_Laire


list : List Attributes
list =
    [ { id = Markus_Laire
      , name = "Markus_Laire"
      , twitter = ""
      , picture = "people/markus-laire.png"
      , github = "malaire"
      , url = ""
      }
    , { id = Suresh_Yadali
      , name = "Suresh Yadali"
      , twitter = ""
      , picture = "people/suresh-yadali.jpeg"
      , github = "yadalis"
      , url = ""
      }
    , { id = Tomas_Latal
      , name = "Tomáš Látal"
      , twitter = ""
      , picture = "people/tomas-latal.jpeg"
      , github = "kraklin"
      , url = ""
      }
    , { id = Jan_Hrcek
      , name = "Jan Hrcek"
      , twitter = ""
      , picture = "people/jan-hrcek.jpeg"
      , github = "jhrcek"
      , url = ""
      }
    , { id = Miyamoen
      , name = "Miyamoen"
      , twitter = ""
      , picture = "people/miyamoen.jpeg"
      , github = "miyamoen"
      , url = ""
      }
    , { id = Martin_Feineis
      , name = "Martin Feineis"
      , twitter = ""
      , picture = "people/martin-feineis.png"
      , github = ""
      , url = "http://canena.de/"
      }
    , { id = Justin_Herrick
      , name = "Justin Herrick"
      , twitter = ""
      , picture = "people/justin-herrick.jpeg"
      , github = "jah2488"
      , url = "http://justinherrick.com/"
      }
    , { id = Remi_Van_Keisbelck
      , name = "Remi Van Keisbelck"
      , twitter = ""
      , picture = "people/remi-van-keisbelck.png"
      , github = "vankeisb"
      , url = "http://www.rvkb.com/"
      }
    , { id = Jo_Wood
      , name = "Jo Wood"
      , twitter = ""
      , picture = "people/jo-wood.png"
      , github = "tennety"
      , url = "https://www.gicentre.net/"
      }
    , { id = Chandu_Tennety
      , name = "Chandu Tennety"
      , twitter = ""
      , picture = "people/chandu-tennety.jpeg"
      , github = "tennety"
      , url = ""
      }
    , { id = Fabian_Kirchner
      , name = "Fabian Kirchner"
      , twitter = ""
      , picture = "people/fabian-kirchner.jpeg"
      , github = "kirchner"
      , url = ""
      }
    , { id = Sosuke
      , name = "Sosuke"
      , twitter = ""
      , picture = "people/sosuke.png"
      , github = "nikueaterso"
      , url = ""
      }
    , { id = Matias_Klemola
      , name = "Matias Klemola"
      , twitter = ""
      , picture = "people/matias-klemola.jpeg"
      , github = "klazuka"
      , url = ""
      }
    , { id = Rogerio_Chaves
      , name = "Rogério Chaves"
      , twitter = ""
      , picture = "people/rogerio-chaves.jpeg"
      , github = "rogeriochaves"
      , url = ""
      }
    , { id = Keith_Lazuka
      , name = "Keith Lazuka"
      , twitter = ""
      , picture = "people/keith-lazuka.png"
      , github = "klazuka"
      , url = ""
      }
    , { id = Jane_G
      , name = "Jane G."
      , twitter = ""
      , picture = "people/jane-g.png"
      , github = "isomoar"
      , url = ""
      }
    , { id = Andreas_Hultgren
      , name = "Andreas Hultgren"
      , twitter = ""
      , picture = "people/andreas-hultgren.jpeg"
      , github = "ahultgren"
      , url = ""
      }
    , { id = Matthew_Buscemi
      , name = "Matthew Buscemi"
      , twitter = ""
      , picture = "people/matthew-buscemi.jpeg"
      , github = "mbuscemi"
      , url = ""
      }
    , { id = Michael_Bylstra
      , name = "Michael Bylstra"
      , twitter = ""
      , picture = "people/michael-bylstra.jpeg"
      , github = ""
      , url = ""
      }
    , { id = Austin_Bingham
      , name = "Austin Bingham"
      , twitter = "austin_bingham"
      , picture = "people/austin-bingham.png"
      , github = "abingham"
      , url = ""
      }
    , { id = Brian_Hicks
      , name = "Brian Hicks"
      , twitter = "brianhicks"
      , picture = "people/brian-hicks.jpg"
      , github = "BrianHicks"
      , url = ""
      }
    , { id = Luca_Mugnaini
      , name = "Luca Mugnaini"
      , twitter = "luca_mug"
      , picture = "people/luca-mugnaini.jpg"
      , github = "lucamug"
      , url = ""
      }
    , { id = Richard_Feldman
      , name = "Richard Feldman"
      , twitter = "rtfeldman"
      , picture = "people/richard-feldman.jpg"
      , github = "rtfeldman"
      , url = ""
      }
    , { id = Evan_Czaplicki
      , name = "Evan Czaplicki"
      , twitter = "czaplic"
      , picture = "people/evan-czaplicki.jpeg"
      , github = "evancz"
      , url = ""
      }
    , { id = Jakub_Hampl
      , name = "Jakub Hampl"
      , twitter = ""
      , picture = "people/jakub-hampl.png"
      , github = ""
      , url = ""
      }
    , { id = Greg_Ziegan
      , name = "Greg Ziegan"
      , twitter = ""
      , picture = "people/greg-ziegan.png"
      , github = ""
      , url = ""
      }
    , { id = Matthew_Griffith
      , name = "Matthew Griffith"
      , twitter = "mech_elephant"
      , picture = "people/matthew-griffith.jpg"
      , github = "mdgriffith"
      , url = ""
      }
    , { id = Noah_Zachary_Gordon
      , name = "Noah Zachary Gordon"
      , twitter = ""
      , picture = "people/noah-zachary-gordon.png"
      , github = ""
      , url = ""
      }
    , { id = Tereza_Sokol
      , name = "Tereza Sokol"
      , twitter = "terezk_a"
      , picture = "people/tereza-sokol.png"
      , github = "terezka"
      , url = ""
      }
    , { id = Peter_Szerzo
      , name = "Peter Szerzo"
      , twitter = "peterszerzo"
      , picture = "people/peter-szerzo.png"
      , github = ""
      , url = "http://www.peterszerzo.com/"
      }
    , { id = Sebastien_Creme
      , name = "Sébastien Crème"
      , twitter = ""
      , picture = "people/sebastien-creme.png"
      , github = ""
      , url = ""
      }
    , { id = Noah_Hall
      , name = "Noah Hall"
      , twitter = "eeue56"
      , picture = "people/noah-hall.png"
      , github = "eeue56"
      , url = ""
      }
    , { id = Jeff_Schomay
      , name = "Jeff Schomay"
      , twitter = ""
      , picture = "people/jeff-schomay.jpg"
      , github = ""
      , url = ""
      }
    , { id = Tomek_Wiszniewski
      , name = "Tomek Wiszniewski"
      , twitter = ""
      , picture = "people/tomek-wiszniewski.png"
      , github = ""
      , url = ""
      }
    , { id = Vincent_Billey
      , name = "Vincent Billey"
      , twitter = ""
      , picture = "people/vincent-billey.png"
      , github = ""
      , url = ""
      }
    , { id = Martin_Janiczek
      , name = "Martin Janiczek"
      , twitter = "janiczek"
      , picture = "people/martin-janiczek.jpeg"
      , github = "Janiczek"
      , url = ""
      }
    , { id = Matthias_Rella
      , name = "Matthias Rella"
      , twitter = "my_rho"
      , picture = "people/matthias-rella.png"
      , github = "myrho"
      , url = ""
      }
    , { id = Robin_Heggelund_Hansen
      , name = "Robin Heggelund Hansen"
      , twitter = "robheghan"
      , picture = "people/robin-heggelund-hansen.png"
      , github = "Skinney"
      , url = ""
      }
    , { id = Christophe_Benz
      , name = "Christophe Benz"
      , twitter = ""
      , picture = "people/christophe-benz.png"
      , github = ""
      , url = ""
      }
    , { id = Amitai_Burstein
      , name = "Amitai Burstein"
      , twitter = ""
      , picture = "people/amitai-burstein.png"
      , github = ""
      , url = ""
      }
    , { id = Josh_Steiner
      , name = "Josh Steiner"
      , twitter = ""
      , picture = "people/josh-steiner.png"
      , github = ""
      , url = ""
      }
    , { id = Mario_Rogic
      , name = "Mario Rogic"
      , twitter = "realmario"
      , picture = "people/mario-rogic.jpg"
      , github = "supermario"
      , url = ""
      }
    , { id = Andrey_Kuzmin
      , name = "Andrey Kuzmin"
      , twitter = "unsoundscapes"
      , picture = "people/andrey-kuzmin.jpg"
      , github = "w0rm"
      , url = "http://unsoundscapes.com/"
      }
    , { id = Jesse_Thompson
      , name = "Jesse Thompson"
      , twitter = ""
      , picture = "people/jesse-thompson.png"
      , github = ""
      , url = ""
      }
    , { id = Grant_Maki
      , name = "Grant Maki"
      , twitter = ""
      , picture = "people/grant-maki.jpg"
      , github = ""
      , url = ""
      }
    , { id = Ravi_Chugh
      , name = "Ravi Chugh"
      , twitter = ""
      , picture = "people/ravi-chugh.jpg"
      , github = ""
      , url = ""
      }
    , { id = Anthony_Deschamps
      , name = "Anthony Deschamps"
      , twitter = ""
      , picture = "people/anthony-deschamps.jpeg"
      , github = ""
      , url = ""
      }
    , { id = Dillon_Kearns
      , name = "Dillon Kearns"
      , twitter = ""
      , picture = "people/dillon-kearns.jpeg"
      , github = ""
      , url = ""
      }
    , { id = Sam_Rowe
      , name = "Sam Rowe"
      , twitter = ""
      , picture = "people/sam-rowe.jpg"
      , github = ""
      , url = ""
      }
    , { id = Ally_Kelly_McKnight
      , name = "Ally Kelly McKnight"
      , twitter = ""
      , picture = "people/ally-kelly-mcknight.jpeg"
      , github = ""
      , url = ""
      }
    , { id = Jonas_Coch
      , name = "Jonas Coch"
      , twitter = ""
      , picture = "people/jonas-coch.png"
      , github = ""
      , url = ""
      }
    , { id = Tessa_Kelly
      , name = "Tessa Kelly"
      , twitter = "t_kelly9"
      , picture = "people/tessa-kelly.png"
      , github = "tesk9"
      , url = ""
      }
    , { id = Murphy_Randle
      , name = "Murphy Randle"
      , twitter = "splodingsocks"
      , picture = "people/murphy-randle.jpeg"
      , github = "splodingsocks"
      , url = ""
      }
    , { id = Luke_Westby
      , name = "Luke Westby"
      , twitter = ""
      , picture = "people/luke-westby.jpeg"
      , github = "lukewestby"
      , url = ""
      }
    , { id = Jeremy_Fairbank
      , name = "Jeremy Fairbank"
      , twitter = ""
      , picture = "people/jeremy-fairbank.jpg"
      , github = ""
      , url = ""
      }
    , { id = Peter_Zingg
      , name = "Peter Zingg"
      , twitter = ""
      , picture = "people/peter-zingg.jpg"
      , github = ""
      , url = ""
      }
    , { id = Ossi_Hanhinen
      , name = "Ossi Hanhinen"
      , twitter = ""
      , picture = "people/ossi-hanhinen.jpeg"
      , github = ""
      , url = ""
      }
    , { id = Abadi_Kurniawaan
      , name = "Abadi Kurniawaan"
      , twitter = ""
      , picture = "people/abadi-kurniawan.jpg"
      , github = ""
      , url = ""
      }
    , { id = Joel_Quenneville
      , name = "Joël Quenneville"
      , twitter = "joelquen"
      , picture = "people/joel-quenneville.jpeg"
      , github = "JoelQ"
      , url = ""
      }
    , { id = Jessica_Kerr
      , name = "Jessica Kerr"
      , twitter = ""
      , picture = "people/jessica-kerr.jpg"
      , github = ""
      , url = ""
      }
    , { id = Yosuke_Torii
      , name = "Yosuke Torii"
      , twitter = "jinjor"
      , picture = "people/yosuke-torii.jpg"
      , github = "jinjor"
      , url = ""
      }
    , { id = Ilias_Van_Peer
      , name = "Ilias Van Peer"
      , twitter = "zwilias"
      , picture = "people/ilias-van-peer.png"
      , github = "zwilias"
      , url = ""
      }
    , { id = James_Carlson
      , name = "James Carlson"
      , twitter = "epsilon2718"
      , picture = "people/james-carlson.png"
      , github = "jxxcarlson"
      , url = "https://jxxcarlson.github.io/"
      }
    , { id = Christopher_Bertels
      , name = "Christopher Bertels"
      , twitter = "bakkdoor"
      , picture = "people/christopher-bertels.png"
      , github = "bakkdoor"
      , url = "https://syncrypt.space/"
      }
    , { id = Alexander_Kachkaev
      , name = "Alexander Kachkaev"
      , twitter = "gicentre"
      , picture = "people/alexander-kachkaev.png"
      , github = "kachkaev"
      , url = ""
      }
    , { id = Ian_Mackenzie
      , name = "Ian Mackenzie"
      , twitter = "https://twitter.com/ianemackenzie"
      , picture = "people/ian-mackenzie.png"
      , github = "ianmackenzie"
      , url = "https://github.com/opensolid"
      }
    , { id = Celine_Martinet_Sanchez
      , name = "Céline Martinet Sanchez"
      , twitter = "celine-m-s"
      , picture = "people/celine-martinet-sanchez.png"
      , github = ""
      , url = "http://www.hello-birds.com/"
      }
    , { id = Ju_Liu
      , name = "Ju Liu"
      , twitter = "arkh4m"
      , picture = "people/ju-liu.png"
      , github = ""
      , url = "https://github.com/Arkham"
      }
    , { id = Mark_Skipper
      , name = "Mark Skipper"
      , twitter = ""
      , picture = "people/mark-skipper.png"
      , github = ""
      , url = ""
      }
    , { id = Decio_Ferreira
      , name = "Décio Ferreira"
      , twitter = ""
      , picture = "people/decio-ferreira.jpeg"
      , github = ""
      , url = ""
      }
    , { id = Thibaut_Assus
      , name = "Thibaut Assus"
      , twitter = ""
      , picture = "people/thibaut-assus.jpeg"
      , github = ""
      , url = ""
      }
    , { id = Emma_Cunningham
      , name = "Emma Cunningham"
      , twitter = "emmatcu"
      , picture = "people/emma-cunningham.png"
      , github = "emmacunningham"
      , url = ""
      }
    , { id = Paul_Sonnentag
      , name = "Paul Sonnentag"
      , twitter = "paulsonnentag"
      , picture = "people/paul-sonnentag.png"
      , github = "paulsonnentag"
      , url = "http://paulsonnentag.com/"
      }
    , { id = Justin_Mimbs
      , name = "Justin Mimbs"
      , twitter = "justinmimbs"
      , picture = "people/justin-mimbs.png"
      , github = "justinmimbs"
      , url = ""
      }
    , { id = Roman_Potashow
      , name = "Roman Potashow"
      , twitter = ""
      , picture = "people/roman-potashow.jpeg"
      , github = "justgook"
      , url = ""
      }
    , { id = Nils_Eriksson
      , name = "Nils Eriksson"
      , twitter = ""
      , picture = "people/nils-eriksson.jpg"
      , github = ""
      , url = ""
      }
    , { id = Erlend_Hamberg
      , name = "Erlend Hamberg"
      , twitter = ""
      , picture = "people/erlend-hamberg.png"
      , github = ""
      , url = ""
      }
    , { id = Felix_Lamouroux
      , name = "Felix Lamouroux"
      , twitter = ""
      , picture = "people/felix-lamouroux.jpg"
      , github = ""
      , url = ""
      }
    , { id = Magnus_Rundberget
      , name = "Magnus Rundberget"
      , twitter = ""
      , picture = "people/magnus-rundberget.jpg"
      , github = ""
      , url = ""
      }
    , { id = David_Ed_Mellum
      , name = "David Ed Mellum"
      , twitter = ""
      , picture = "people/david-ed-mellum.jpg"
      , github = ""
      , url = ""
      }
    , { id = Remi_Lefevre
      , name = "Rémi Lefèvre"
      , twitter = ""
      , picture = "people/remi-lefevre.png"
      , github = "dmy"
      , url = ""
      }
    , { id = Mats_Stijlaart
      , name = "Mats Stijlaart"
      , twitter = ""
      , picture = "people/mats-stijlaart.png"
      , github = "stil4m"
      , url = "http://stil4m.github.io/"
      }
    , { id = Francesco_Orsenigo
      , name = "Francesco Orsenigo"
      , twitter = ""
      , picture = "people/francesco-orsenigo.png"
      , github = "xarvh"
      , url = ""
      }
    , { id = Halohalospecial
      , name = "halohalospecial"
      , twitter = ""
      , picture = ""
      , github = "halohalospecial"
      , url = ""
      }
    , { id = Roman_Frolow
      , name = "Roman Frołow"
      , twitter = ""
      , picture = "people/roman-frolow.jpeg"
      , github = "rofrol"
      , url = ""
      }
    , { id = Ronan
      , name = "Rónán"
      , twitter = ""
      , picture = "people/ronan.jpeg"
      , github = "ronanyeah"
      , url = ""
      }
    ]
