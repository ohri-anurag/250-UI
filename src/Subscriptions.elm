port module Subscriptions exposing (..)


import Maybe exposing (andThen)
import String exposing (toInt)


import Model exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
  messageReceiver (\str ->
    case model of
      WaitingForPlayers ->
        case andThen intToPlayerIndex (toInt str) of
          Just index ->
            BeginGame index

          Nothing ->
            NoOp

      _ -> NoOp
  )




port messageReceiver : (String -> msg) -> Sub msg