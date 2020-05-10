module Subscriptions exposing (..)


import Json.Decode exposing (decodeString)
import Maybe exposing (andThen)
import String exposing (toInt)


import Model exposing (Model, Msg)
import Decoders exposing (receiveMessage)


subscriptions : Model -> Sub Msg
subscriptions _ = receiveMessage
