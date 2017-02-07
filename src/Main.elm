module Main exposing (..)

import App.View exposing (view)
import App.State exposing (init, update, subscriptions)
import App.Types exposing (Model, Msg)
import Html exposing (program)


main : Program Never Model Msg
main =
    program { view = view, init = init, update = update, subscriptions = subscriptions }
