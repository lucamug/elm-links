module Shared exposing (commandToCloseModal)

import Browser.Navigation
import CommonRoute
import Route


commandToCloseModal :
    { a
        | filter : String
        , key : Browser.Navigation.Key
    }
    -> Cmd msg
commandToCloseModal { filter, key } =
    Browser.Navigation.pushUrl key <|
        CommonRoute.toStringAndHash Route.conf <|
            Route.routeToRestoreFilter filter
