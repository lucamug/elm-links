module Update exposing (update)

import Browser
import Browser.Navigation
import CommonRoute
import Keyboard
import Model
import Msg
import Route
import Shared
import Url
import Utils


update : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        Msg.UrlChanged url ->
            let
                route =
                    CommonRoute.fromUrl Route.conf url

                filter =
                    case route of
                        Route.Filter filter_ ->
                            filter_

                        Route.Empty ->
                            ""

                        _ ->
                            model.filter
            in
            ( { model | url = url, filter = filter }, Cmd.none )

        Msg.Click data ->
            if data.id1 == "cover" then
                ( model
                , Shared.commandToCloseModal model
                )

            else
                ( model, Cmd.none )

        Msg.KeyUp key ->
            if Keyboard.rawValue key == "Escape" then
                ( model
                , Shared.commandToCloseModal model
                )

            else
                ( model, Cmd.none )

        Msg.PageInTopArea state ->
            ( { model | pageInTopArea = state }, Cmd.none )

        Msg.OnResize x _ ->
            let
                newQuantity =
                    x // floor model.squareWidth

                newQuantity_ =
                    if newQuantity > 1 then
                        newQuantity

                    else
                        1
            in
            ( { model | width = x, squareQuantity = newQuantity_ }, Cmd.none )

        Msg.IncreaseSquareQuantity ->
            let
                newQuantity =
                    model.squareQuantity + 1
            in
            ( { model
                | squareQuantity = newQuantity
                , squareWidth = toFloat model.width / toFloat newQuantity
              }
            , Cmd.none
            )

        Msg.DecreaseSquareQuantity ->
            let
                newQuantity =
                    if model.squareQuantity > 1 then
                        model.squareQuantity - 1

                    else
                        model.squareQuantity
            in
            ( { model
                | squareQuantity = newQuantity
                , squareWidth = toFloat model.width / toFloat newQuantity
              }
            , Cmd.none
            )

        Msg.ToggleColorMode ->
            ( { model
                | colorMode =
                    case model.colorMode of
                        Model.Day ->
                            Model.Night

                        _ ->
                            Model.Day
              }
            , Cmd.none
            )

        Msg.ToggleLayoutMode ->
            ( { model
                | layoutMode =
                    case model.layoutMode of
                        Model.Grid ->
                            Model.List

                        _ ->
                            Model.Grid
              }
            , Cmd.none
            )

        Msg.ChangeFilter filter ->
            ( { model | filter = filter }
            , Browser.Navigation.pushUrl model.key <|
                CommonRoute.toStringAndHash
                    Route.conf
                <|
                    if filter == "" then
                        Route.Empty

                    else
                        Route.Filter <| Utils.encode filter
            )
