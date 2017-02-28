module Main exposing (..)

import App.View exposing (view)
import App.State exposing (init, update, subscriptions)
import App.Types exposing (Model, Msg, User)
import Html exposing (programWithFlags)
import Json.Decode exposing (Value)


main : Program Value Model Msg
main =
    programWithFlags { view = view, init = init, update = update, subscriptions = subscriptions }
