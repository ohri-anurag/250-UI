port module Subscriptions exposing (..)


import Json.Decode exposing (decodeString)
import Maybe exposing (andThen)
import String exposing (toInt)


import Model exposing (..)
import SharedData exposing (decodeInitGameState)

subscriptions : Model -> Sub Msg
subscriptions model =
  messageReceiver (\str ->
    case decodeString decodeInitGameState str of
      Ok initGameState ->
        BeginGame initGameState.index initGameState.cards

      _ ->
        NoOp
    --case model of
    --  WaitingForPlayers ->
    --    case andThen intToPlayerIndex (toInt str) of
    --      Just index ->
    --        BeginGame index

    --      Nothing ->
    --        NoOp

    --  _ -> NoOp
  )




port messageReceiver : (String -> msg) -> Sub msg