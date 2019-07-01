module Main exposing (main)

import Browser
import Browser.Events
import Init
import Keyboard
import Model
import Msg
import Port
import Update
import View


main : Program Init.Flags Model.Model Msg.Msg
main =
    Browser.application
        { init = Init.init
        , view = View.view
        , update = Update.update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onResize Msg.OnResize
                    , Port.pageInTopArea Msg.PageInTopArea
                    , Keyboard.ups Msg.KeyUp
                    ]
        , onUrlRequest = Msg.LinkClicked
        , onUrlChange = Msg.UrlChanged
        }
