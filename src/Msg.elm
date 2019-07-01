module Msg exposing
    ( ClickData
    , Msg(..)
    )

import Browser
import Keyboard
import Url


type alias ClickData =
    { id1 : String
    , id2 : String
    , id3 : String
    , id4 : String
    , id5 : String
    }


type Msg
    = Click ClickData
    | OnResize Int Int
    | ToggleColorMode
    | ToggleLayoutMode
    | IncreaseSquareQuantity
    | DecreaseSquareQuantity
    | ChangeFilter String
    | PageInTopArea Bool
    | KeyUp Keyboard.RawKey
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
