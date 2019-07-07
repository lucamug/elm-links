module Data.People exposing
    ( Attributes
    , Id
    , WithQuantity
    , empty
    , list
    )

import Utils


type alias Attributes =
    { -- id : Id -- The type constructor
      id : String
    , name : String -- Name, usually the same as the type constructor
    , picture : String -- Picture that represent the person
    , twitter : String -- Twitter handle
    , github : String -- Github handle
    , url : String -- Link to a personal homepage
    , country : String
    }


type alias Id =
    String


type alias WithQuantity =
    { lookup : Attributes
    , quantity : Int
    }


empty : Attributes
empty =
    { id = ""
    , name = ""
    , picture = ""
    , twitter = ""
    , github = ""
    , url = ""
    , country = ""
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
    [ { id = "Elm"
      , name = "Elm Github Account"
      , twitter = ""
      , picture = "svg/elm_logo.svg"
      , github = "elm"
      , url = "https://github.com/elm"
      , country = ""
      }
    , { id = "Elm_Community"
      , name = "Elm Community Github Account"
      , twitter = ""
      , picture = "people/elm-community.png"
      , github = "elm-community"
      , url = "http://elm-community.org/"
      , country = ""
      }
    , { id = "Keith_Lazuka"
      , name = "Keith Lazuka"
      , twitter = ""
      , picture = "people/keith-lazuka.png"
      , github = "klazuka"
      , url = ""
      , country = ""
      }
    , { id = "Jeremy_Brown"
      , name = "Jeremy H. Brown"
      , twitter = ""
      , picture = "people/jeremy-brown.jpeg"
      , github = "jhbrown94"
      , url = ""
      , country = ""
      }
    , { id = "Teodor_Lunaas_Heggelund"
      , name = "Teodor Lunaas Heggelund"
      , twitter = ""
      , picture = "people/teodor-lunaas-heggelund.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Einar_Host"
      , name = "Einar W. Høst"
      , twitter = ""
      , picture = "people/einar-w-host.jpg"
      , github = "einarwh"
      , url = ""
      , country = ""
      }
    , { id = "Dan_Abrams"
      , name = "Dan Abrams"
      , twitter = ""
      , picture = "people/dan-abrams.jpg"
      , github = "danabrams"
      , url = ""
      , country = ""
      }
    , { id = "Jonas_Berdal"
      , name = "Jonas Berdal"
      , twitter = "people/jonas-berdal.jpeg"
      , picture = ""
      , github = "jonasberdal"
      , url = ""
      , country = ""
      }
    , { id = "Manuel_Fuchs"
      , name = "Manuel Fuchs"
      , twitter = ""
      , picture = "people/manuel-fuchs.jpeg"
      , github = "Malax"
      , url = ""
      , country = ""
      }
    , { id = "Kris_Jenkins"
      , name = "Kris Jenkins"
      , twitter = ""
      , picture = "people/kris-jenkins.jpg"
      , github = "krisajenkins"
      , url = ""
      , country = ""
      }
    , { id = "Emma_Tsujimoto_Cunningham"
      , name = "Emma Tsujimoto Cunningham"
      , twitter = ""
      , picture = "people/emma-tsujimoto-cunningham.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Filip_Haglund"
      , name = "Filip Haglund"
      , twitter = ""
      , picture = "people/filip-haglund.jpeg"
      , github = "drathier"
      , url = ""
      , country = ""
      }
    , { id = "Viktor_Tymoshenko"
      , name = "Viktor Tymoshenko"
      , twitter = ""
      , picture = "people/viktor-tymoshenko.jpg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Anna_Bansaghi"
      , name = "Anna Bansaghi"
      , twitter = ""
      , picture = "people/anna-bansaghi.jpeg"
      , github = "annaghi"
      , url = ""
      , country = ""
      }
    , { id = "James_Kolce"
      , name = "James Kolce"
      , twitter = ""
      , picture = "people/james-kolce.jpeg"
      , github = "jameskolce"
      , url = ""
      , country = ""
      }
    , { id = "Markus_Laire"
      , name = "Markus Laire"
      , twitter = ""
      , picture = "people/markus-laire.png"
      , github = "malaire"
      , url = ""
      , country = ""
      }
    , { id = "James_Gary"
      , name = "James Gary"
      , twitter = ""
      , picture = "people/james-gary.jpeg"
      , github = "jamesgary"
      , url = "http://codingcats.com/"
      , country = ""
      }
    , { id = "Suresh_Yadali"
      , name = "Suresh Yadali"
      , twitter = ""
      , picture = "people/suresh-yadali.jpeg"
      , github = "yadalis"
      , url = ""
      , country = ""
      }
    , { id = "Tomas_Latal"
      , name = "Tomáš Látal"
      , twitter = ""
      , picture = "people/tomas-latal.jpeg"
      , github = "kraklin"
      , url = ""
      , country = ""
      }
    , { id = "Jan_Hrcek"
      , name = "Jan Hrcek"
      , twitter = ""
      , picture = "people/jan-hrcek.jpeg"
      , github = "jhrcek"
      , url = ""
      , country = ""
      }
    , { id = "Miyamoen"
      , name = "Miyamoen"
      , twitter = ""
      , picture = "people/miyamoen.jpeg"
      , github = "miyamoen"
      , url = ""
      , country = ""
      }
    , { id = "Martin_Feineis"
      , name = "Martin Feineis"
      , twitter = ""
      , picture = "people/martin-feineis.png"
      , github = ""
      , url = "http://canena.de/"
      , country = ""
      }
    , { id = "Justin_Herrick"
      , name = "Justin Herrick"
      , twitter = ""
      , picture = "people/justin-herrick.jpeg"
      , github = "jah2488"
      , url = "http://justinherrick.com/"
      , country = ""
      }
    , { id = "Remi_Van_Keisbelck"
      , name = "Remi Van Keisbelck"
      , twitter = ""
      , picture = "people/remi-van-keisbelck.png"
      , github = "vankeisb"
      , url = "http://www.rvkb.com/"
      , country = ""
      }
    , { id = "Jo_Wood"
      , name = "Jo Wood"
      , twitter = ""
      , picture = "people/jo-wood.png"
      , github = "tennety"
      , url = "https://www.gicentre.net/"
      , country = ""
      }
    , { id = "Chandu_Tennety"
      , name = "Chandu Tennety"
      , twitter = ""
      , picture = "people/chandu-tennety.jpeg"
      , github = "tennety"
      , url = ""
      , country = ""
      }
    , { id = "Fabian_Kirchner"
      , name = "Fabian Kirchner"
      , twitter = ""
      , picture = "people/fabian-kirchner.jpeg"
      , github = "kirchner"
      , url = ""
      , country = ""
      }
    , { id = "Sosuke"
      , name = "Sosuke"
      , twitter = ""
      , picture = "people/sosuke.png"
      , github = "nikueaterso"
      , url = ""
      , country = ""
      }
    , { id = "Matias_Klemola"
      , name = "Matias Klemola"
      , twitter = ""
      , picture = "people/matias-klemola.jpeg"
      , github = "klazuka"
      , url = ""
      , country = ""
      }
    , { id = "Rogerio_Chaves"
      , name = "Rogério Chaves"
      , twitter = ""
      , picture = "people/rogerio-chaves.jpeg"
      , github = "rogeriochaves"
      , url = ""
      , country = ""
      }
    , { id = "Jane_G"
      , name = "Jane G."
      , twitter = ""
      , picture = "people/jane-g.png"
      , github = "isomoar"
      , url = ""
      , country = ""
      }
    , { id = "Andreas_Hultgren"
      , name = "Andreas Hultgren"
      , twitter = ""
      , picture = "people/andreas-hultgren.jpeg"
      , github = "ahultgren"
      , url = ""
      , country = ""
      }
    , { id = "Matthew_Buscemi"
      , name = "Matthew Buscemi"
      , twitter = ""
      , picture = "people/matthew-buscemi.jpeg"
      , github = "mbuscemi"
      , url = ""
      , country = ""
      }
    , { id = "Michael_Bylstra"
      , name = "Michael Bylstra"
      , twitter = ""
      , picture = "people/michael-bylstra.jpeg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Austin_Bingham"
      , name = "Austin Bingham"
      , twitter = "austin_bingham"
      , picture = "people/austin-bingham.png"
      , github = "abingham"
      , url = ""
      , country = ""
      }
    , { id = "Brian_Hicks"
      , name = "Brian Hicks"
      , twitter = "brianhicks"
      , picture = "people/brian-hicks.jpg"
      , github = "BrianHicks"
      , url = ""
      , country = ""
      }
    , { id = "Luca_Mugnaini"
      , name = "Luca Mugnaini"
      , twitter = "luca_mug"
      , picture = "people/luca-mugnaini.jpg"
      , github = "lucamug"
      , url = ""
      , country = ""
      }
    , { id = "Richard_Feldman"
      , name = "Richard Feldman"
      , twitter = "rtfeldman"
      , picture = "people/richard-feldman.jpg"
      , github = "rtfeldman"
      , url = ""
      , country = ""
      }
    , { id = "Evan_Czaplicki"
      , name = "Evan Czaplicki"
      , twitter = "czaplic"
      , picture = "people/evan-czaplicki.jpeg"
      , github = "evancz"
      , url = ""
      , country = ""
      }
    , { id = "Jakub_Hampl"
      , name = "Jakub Hampl"
      , twitter = ""
      , picture = "people/jakub-hampl.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Greg_Ziegan"
      , name = "Greg Ziegan"
      , twitter = ""
      , picture = "people/greg-ziegan.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Matthew_Griffith"
      , name = "Matthew Griffith"
      , twitter = "mech_elephant"
      , picture = "people/matthew-griffith.jpg"
      , github = "mdgriffith"
      , url = ""
      , country = ""
      }
    , { id = "Noah_Zachary_Gordon"
      , name = "Noah Zachary Gordon"
      , twitter = ""
      , picture = "people/noah-zachary-gordon.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Tereza_Sokol"
      , name = "Tereza Sokol"
      , twitter = "terezk_a"
      , picture = "people/tereza-sokol.png"
      , github = "terezka"
      , url = ""
      , country = ""
      }
    , { id = "Peter_Szerzo"
      , name = "Peter Szerzo"
      , twitter = "peterszerzo"
      , picture = "people/peter-szerzo.png"
      , github = ""
      , url = "http://www.peterszerzo.com/"
      , country = ""
      }
    , { id = "Sebastien_Creme"
      , name = "Sébastien Crème"
      , twitter = ""
      , picture = "people/sebastien-creme.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Noah_Hall"
      , name = "Noah Hall"
      , twitter = "eeue56"
      , picture = "people/noah-hall.png"
      , github = "eeue56"
      , url = ""
      , country = ""
      }
    , { id = "Jeff_Schomay"
      , name = "Jeff Schomay"
      , twitter = ""
      , picture = "people/jeff-schomay.jpg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Tomek_Wiszniewski"
      , name = "Tomek Wiszniewski"
      , twitter = "architectcodes"
      , picture = "people/tomek-wiszniewski.png"
      , github = "architectcodes"
      , url = "http://architect.codes/"
      , country = ""
      }
    , { id = "Vincent_Billey"
      , name = "Vincent Billey"
      , twitter = ""
      , picture = "people/vincent-billey.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Martin_Janiczek"
      , name = "Martin Janiczek"
      , twitter = "janiczek"
      , picture = "people/martin-janiczek.jpeg"
      , github = "Janiczek"
      , url = ""
      , country = ""
      }
    , { id = "Matthias_Rella"
      , name = "Matthias Rella"
      , twitter = "my_rho"
      , picture = "people/matthias-rella.png"
      , github = "myrho"
      , url = ""
      , country = ""
      }
    , { id = "Robin_Heggelund_Hansen"
      , name = "Robin Heggelund Hansen"
      , twitter = "robheghan"
      , picture = "people/robin-heggelund-hansen.png"
      , github = "Skinney"
      , url = ""
      , country = ""
      }
    , { id = "Christophe_Benz"
      , name = "Christophe Benz"
      , twitter = ""
      , picture = "people/christophe-benz.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Amitai_Burstein"
      , name = "Amitai Burstein"
      , twitter = ""
      , picture = "people/amitai-burstein.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Josh_Steiner"
      , name = "Josh Steiner"
      , twitter = ""
      , picture = "people/josh-steiner.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Mario_Rogic"
      , name = "Mario Rogic"
      , twitter = "realmario"
      , picture = "people/mario-rogic.jpg"
      , github = "supermario"
      , url = ""
      , country = ""
      }
    , { id = "Andrey_Kuzmin"
      , name = "Andrey Kuzmin"
      , twitter = "unsoundscapes"
      , picture = "people/andrey-kuzmin.jpg"
      , github = "w0rm"
      , url = "http://unsoundscapes.com/"
      , country = ""
      }
    , { id = "Jesse_Thompson"
      , name = "Jesse Thompson"
      , twitter = ""
      , picture = "people/jesse-thompson.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Grant_Maki"
      , name = "Grant Maki"
      , twitter = ""
      , picture = "people/grant-maki.jpg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Ravi_Chugh"
      , name = "Ravi Chugh"
      , twitter = ""
      , picture = "people/ravi-chugh.jpg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Anthony_Deschamps"
      , name = "Anthony Deschamps"
      , twitter = ""
      , picture = "people/anthony-deschamps.jpeg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Dillon_Kearns"
      , name = "Dillon Kearns"
      , twitter = ""
      , picture = "people/dillon-kearns.jpeg"
      , github = "dillonkearns"
      , url = ""
      , country = ""
      }
    , { id = "Sam_Rowe"
      , name = "Sam Rowe"
      , twitter = ""
      , picture = "people/sam-rowe.jpg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Ally_Kelly_McKnight"
      , name = "Ally Kelly McKnight"
      , twitter = ""
      , picture = "people/ally-kelly-mcknight.jpeg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Jonas_Coch"
      , name = "Jonas Coch"
      , twitter = ""
      , picture = "people/jonas-coch.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Tessa_Kelly"
      , name = "Tessa Kelly"
      , twitter = "t_kelly9"
      , picture = "people/tessa-kelly.png"
      , github = "tesk9"
      , url = ""
      , country = ""
      }
    , { id = "Murphy_Randle"
      , name = "Murphy Randle"
      , twitter = "splodingsocks"
      , picture = "people/murphy-randle.jpeg"
      , github = "splodingsocks"
      , url = ""
      , country = ""
      }
    , { id = "Luke_Westby"
      , name = "Luke Westby"
      , twitter = ""
      , picture = "people/luke-westby.jpeg"
      , github = "lukewestby"
      , url = ""
      , country = ""
      }
    , { id = "Jeremy_Fairbank"
      , name = "Jeremy Fairbank"
      , twitter = ""
      , picture = "people/jeremy-fairbank.jpg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Peter_Zingg"
      , name = "Peter Zingg"
      , twitter = ""
      , picture = "people/peter-zingg.jpg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Ossi_Hanhinen"
      , name = "Ossi Hanhinen"
      , twitter = ""
      , picture = "people/ossi-hanhinen.jpeg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Abadi_Kurniawaan"
      , name = "Abadi Kurniawaan"
      , twitter = ""
      , picture = "people/abadi-kurniawan.jpg"
      , github = "abadi199"
      , url = ""
      , country = ""
      }
    , { id = "Joel_Quenneville"
      , name = "Joël Quenneville"
      , twitter = "joelquen"
      , picture = "people/joel-quenneville.jpeg"
      , github = "JoelQ"
      , url = ""
      , country = ""
      }
    , { id = "Jessica_Kerr"
      , name = "Jessica Kerr"
      , twitter = ""
      , picture = "people/jessica-kerr.jpg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Yosuke_Torii"
      , name = "Yosuke Torii"
      , twitter = "jinjor"
      , picture = "people/yosuke-torii.jpg"
      , github = "jinjor"
      , url = ""
      , country = ""
      }
    , { id = "Ilias_Van_Peer"
      , name = "Ilias Van Peer"
      , twitter = "zwilias"
      , picture = "people/ilias-van-peer.png"
      , github = "zwilias"
      , url = ""
      , country = ""
      }
    , { id = "James_Carlson"
      , name = "James Carlson"
      , twitter = "epsilon2718"
      , picture = "people/james-carlson.png"
      , github = "jxxcarlson"
      , url = "https://jxxcarlson.github.io/"
      , country = ""
      }
    , { id = "Christopher_Bertels"
      , name = "Christopher Bertels"
      , twitter = "bakkdoor"
      , picture = "people/christopher-bertels.png"
      , github = "bakkdoor"
      , url = "https://syncrypt.space/"
      , country = ""
      }
    , { id = "Alexander_Kachkaev"
      , name = "Alexander Kachkaev"
      , twitter = "gicentre"
      , picture = "people/alexander-kachkaev.png"
      , github = "kachkaev"
      , url = ""
      , country = ""
      }
    , { id = "Ian_Mackenzie"
      , name = "Ian Mackenzie"
      , twitter = "https://twitter.com/ianemackenzie"
      , picture = "people/ian-mackenzie.png"
      , github = "ianmackenzie"
      , url = "https://github.com/opensolid"
      , country = ""
      }
    , { id = "Celine_Martinet_Sanchez"
      , name = "Céline Martinet Sanchez"
      , twitter = "celine-m-s"
      , picture = "people/celine-martinet-sanchez.png"
      , github = ""
      , url = "http://www.hello-birds.com/"
      , country = ""
      }
    , { id = "Ju_Liu"
      , name = "Ju Liu"
      , twitter = "arkh4m"
      , picture = "people/ju-liu.png"
      , github = ""
      , url = "https://github.com/Arkham"
      , country = ""
      }
    , { id = "Mark_Skipper"
      , name = "Mark Skipper"
      , twitter = ""
      , picture = "people/mark-skipper.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Decio_Ferreira"
      , name = "Décio Ferreira"
      , twitter = ""
      , picture = "people/decio-ferreira.jpeg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Thibaut_Assus"
      , name = "Thibaut Assus"
      , twitter = ""
      , picture = "people/thibaut-assus.jpeg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Emma_Cunningham"
      , name = "Emma Cunningham"
      , twitter = "emmatcu"
      , picture = "people/emma-cunningham.png"
      , github = "emmacunningham"
      , url = ""
      , country = ""
      }
    , { id = "Paul_Sonnentag"
      , name = "Paul Sonnentag"
      , twitter = "paulsonnentag"
      , picture = "people/paul-sonnentag.png"
      , github = "paulsonnentag"
      , url = "http://paulsonnentag.com/"
      , country = ""
      }
    , { id = "Justin_Mimbs"
      , name = "Justin Mimbs"
      , twitter = "justinmimbs"
      , picture = "people/justin-mimbs.png"
      , github = "justinmimbs"
      , url = ""
      , country = ""
      }
    , { id = "Roman_Potashow"
      , name = "Roman Potashow"
      , twitter = ""
      , picture = "people/roman-potashow.jpeg"
      , github = "justgook"
      , url = ""
      , country = ""
      }
    , { id = "Nils_Eriksson"
      , name = "Nils Eriksson"
      , twitter = ""
      , picture = "people/nils-eriksson.jpg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Erlend_Hamberg"
      , name = "Erlend Hamberg"
      , twitter = ""
      , picture = "people/erlend-hamberg.png"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Felix_Lamouroux"
      , name = "Felix Lamouroux"
      , twitter = ""
      , picture = "people/felix-lamouroux.jpg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Magnus_Rundberget"
      , name = "Magnus Rundberget"
      , twitter = ""
      , picture = "people/magnus-rundberget.jpg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "David_Ed_Mellum"
      , name = "David Ed Mellum"
      , twitter = ""
      , picture = "people/david-ed-mellum.jpg"
      , github = ""
      , url = ""
      , country = ""
      }
    , { id = "Remi_Lefevre"
      , name = "Rémi Lefèvre"
      , twitter = ""
      , picture = "people/remi-lefevre.png"
      , github = "dmy"
      , url = ""
      , country = ""
      }
    , { id = "Mats_Stijlaart"
      , name = "Mats Stijlaart"
      , twitter = ""
      , picture = "people/mats-stijlaart.png"
      , github = "stil4m"
      , url = "http://stil4m.github.io/"
      , country = ""
      }
    , { id = "Francesco_Orsenigo"
      , name = "Francesco Orsenigo"
      , twitter = ""
      , picture = "people/francesco-orsenigo.png"
      , github = "xarvh"
      , url = ""
      , country = ""
      }
    , { id = "Halohalospecial"
      , name = "halohalospecial"
      , twitter = ""
      , picture = ""
      , github = "halohalospecial"
      , url = ""
      , country = ""
      }
    , { id = "Roman_Frolow"
      , name = "Roman Frołow"
      , twitter = ""
      , picture = "people/roman-frolow.jpeg"
      , github = "rofrol"
      , url = ""
      , country = ""
      }
    , { id = "Ronan"
      , name = "Rónán"
      , twitter = ""
      , picture = "people/ronan.jpeg"
      , github = "ronanyeah"
      , url = ""
      , country = ""
      }
    , { id = "Aaron_VonderHaar"
      , name = "Aaron VonderHaar"
      , twitter = "avh4"
      , picture = "people/aaron-vonderhaar.jpeg"
      , github = "avh4"
      , url = ""
      , country = ""
      }
    , { id = "Alex_Tan"
      , name = "Alex Tan"
      , twitter = ""
      , picture = "people/alex-tan.jpeg"
      , github = "alex-tan"
      , url = ""
      , country = ""
      }
    , { id = "Kadzuya_Okamoto"
      , name = "Kadzuya Okamoto"
      , twitter = ""
      , picture = "people/kadzuya-okamoto.jpeg"
      , github = "arowM"
      , url = "https://arow.info"
      , country = "Japan"
      }
    , { id = "Andy_Andys8"
      , name = "Andy"
      , twitter = "_andys8"
      , picture = "people/andy-andys8.jpeg"
      , github = "andys8"
      , url = ""
      , country = ""
      }
    , { id = "Bill_St_Clair"
      , name = "Bill St. Clair"
      , twitter = ""
      , picture = "people/bill-st-clair.jpeg"
      , github = "billstclair"
      , url = "https://lisplog.org/"
      , country = ""
      }
    , { id = "Dave_Keen"
      , name = "Dave Keen"
      , twitter = ""
      , picture = "people/dave-keen.png"
      , github = "ccapndave"
      , url = "http://www.keendevelopment.ch/"
      , country = ""
      }
    , { id = "Chadtech_Chadtech"
      , name = "Chadtech"
      , twitter = ""
      , picture = "people/chadtech.jpeg"
      , github = "Chadtech"
      , url = "http://www.chadtech.us/"
      , country = ""
      }
    , { id = "GETTO_Systems"
      , name = "GETTO Systems"
      , twitter = ""
      , picture = "people/getto-systems.jpeg"
      , github = "getto-systems"
      , url = "https://www.getto.systems/"
      , country = "Japan"
      }
    , { id = "yumlonne"
      , name = "yumlonne"
      , twitter = ""
      , picture = "people/yumlonne.jpeg"
      , github = "yumlonne"
      , url = "https://www.getto.systems/"
      , country = "Japan"
      }
    , { id = "Justin_Holzmann"
      , name = "Justin Holzmann"
      , twitter = ""
      , picture = "people/justin-holzmann.jpeg"
      , github = "7hoenix"
      , url = "http://8thlight.com/"
      , country = "US, Chicago"
      }
    , { id = "Anatoliy"
      , name = "Anatoliy"
      , twitter = ""
      , picture = "people/anatoliy.jpeg"
      , github = "1602"
      , url = "https://about.ub.io/"
      , country = "UK, Essex"
      }
    , { id = "Tony_Bradley"
      , name = "Tony Bradley"
      , twitter = ""
      , picture = "people/tony-bradley.jpeg"
      , github = "abradley2"
      , url = ""
      , country = "US, Washington DC"
      }
    , { id = "Elm_Explorations"
      , name = "Elm Explorations Github Account"
      , twitter = ""
      , picture = "people/elm-explorations.png"
      , github = "elm-explorations"
      , url = ""
      , country = ""
      }
    , { id = "Abinaya_Sudhir", name = "Abinaya Sudhir", twitter = "", picture = "people/abinaya-sudhir.png", github = "abinayasudhir", url = "", country = "India" }
    , { id = "Achut_Kiran_Cherukuri", name = "achutkiran", twitter = "", picture = "people/achutkiran.png", github = "achutkiran", url = "", country = "" }
    , { id = "adauguet", name = "adauguet", twitter = "", picture = "people/adauguet.png", github = "adauguet", url = "", country = "" }
    , { id = "AdrianRibao", name = "Adrián Ribao", twitter = "", picture = "people/AdrianRibao.jpeg", github = "AdrianRibao", url = "http://www.adrima.es", country = "Spain" }
    , { id = "afidegnum", name = "Kokou Afidegnon", twitter = "", picture = "people/afidegnum.png", github = "afidegnum", url = "", country = "" }
    , { id = "ahstro", name = "Anton Strömkvist", twitter = "", picture = "people/ahstro.jpeg", github = "ahstro", url = "https://ahst.ro", country = "Sweden" }
    , { id = "akoppela", name = "Andrey Koppel", twitter = "", picture = "people/akoppela.png", github = "akoppela", url = "", country = "Indonesia" }
    , { id = "alexanderkiel", name = "alexanderkiel", twitter = "", picture = "people/alexanderkiel.jpeg", github = "alexanderkiel", url = "http://www.alexanderkiel.net", country = "Germany, Leipzig" }
    , { id = "alexkorban", name = "Alex Korban", twitter = "", picture = "people/alexkorban.jpeg", github = "alexkorban", url = "https://korban.net", country = "New Zealand, Wellington" }
    , { id = "allenap", name = "Gavin Panella", twitter = "", picture = "people/allenap.png", github = "allenap", url = "http://allenap.me/", country = "Luxembourg" }
    , { id = "allo-media", name = "Groupe Allo-Media", twitter = "", picture = "people/allo-media.png", github = "allo-media", url = "", country = "" }
    , { id = "altjsus", name = "Гриш∆", twitter = "", picture = "people/altjsus.jpeg", github = "altjsus", url = "http://altjsus.github.io", country = "Russia, Belgorod" }
    , { id = "andre-dietrich", name = "Andre Dietrich", twitter = "", picture = "people/andre-dietrich.jpeg", github = "andre-dietrich", url = "", country = "" }
    , { id = "andrewMacmurray", name = "Andrew MacMurray", twitter = "", picture = "people/andrewMacmurray.jpeg", github = "andrewMacmurray", url = "", country = "UK, London" }
    , { id = "anhmiuhv", name = "Linh Hoang", twitter = "", picture = "people/anhmiuhv.png", github = "anhmiuhv", url = "", country = "US, Atlanta" }
    , { id = "Arkham", name = "Ju Liu", twitter = "", picture = "people/Arkham.jpeg", github = "Arkham", url = "", country = "UK, London" }
    , { id = "arnau", name = "Arnau Siches", twitter = "", picture = "people/arnau.jpeg", github = "arnau", url = "https://www.spoontaneous.net", country = "UK, London" }
    , { id = "arsduo", name = "Alex Koppel", twitter = "", picture = "people/arsduo.jpeg", github = "arsduo", url = "http://alexkoppel.com", country = "US, Chicago" }
    , { id = "arturopala", name = "Artur Opala", twitter = "", picture = "people/arturopala.jpeg", github = "arturopala", url = "", country = "UK, Worthing" }
    , { id = "astynax", name = "Aleksei Pirogov", twitter = "", picture = "people/astynax.png", github = "astynax", url = "", country = "Russia, Moscow" }
    , { id = "AuricSystemsInternational", name = "AuricSystemsInternational", twitter = "", picture = "people/AuricSystemsInternational.png", github = "AuricSystemsInternational", url = "", country = "" }
    , { id = "avh4-experimental", name = "avh4-experimental", twitter = "", picture = "people/avh4-experimental.png", github = "avh4-experimental", url = "", country = "" }
    , { id = "babsballetschool", name = "Babs Balletschool", twitter = "", picture = "people/babsballetschool.png", github = "babsballetschool", url = "https://www.babsballetschool.nl/", country = "Netherland" }
    , { id = "bartavelle", name = "Simon Marechal", twitter = "", picture = "people/bartavelle.jpeg", github = "bartavelle", url = "http://hbtvl.banquise.net", country = "France, Marseille" }
    ]
