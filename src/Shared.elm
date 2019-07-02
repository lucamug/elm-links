module Shared exposing (commandToCloseModal, compareLowerCase)

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


compareLowerCase : String -> String -> Bool
compareLowerCase string1 string2 =
    String.toLower string1 == String.toLower string2
