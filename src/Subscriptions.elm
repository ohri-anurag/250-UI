port module Subscriptions exposing (..)


import Json.Decode exposing (decodeString)
import Maybe exposing (andThen)
import String exposing (toInt)


import Model exposing (..)
import SharedData exposing (gameStateDecoder, biddingDataDecoder)

subscriptions : Model -> Sub Msg
subscriptions model =
  messageReceiver (\str ->
    case model of
      WaitingForPlayers ->
        case decodeString gameStateDecoder str of
          Ok initGameState ->
            BeginGame initGameState

          _ ->
            NoOp

      BiddingRound _ _ _ ->
        case decodeString biddingDataDecoder str of
          Ok biddingData ->
            case biddingData of
              IntermediateBiddingData iBiddingData ->
                NewHighestBid iBiddingData.highestBidder iBiddingData.highestBid

              FinalBiddingData fBiddingData ->
                FinalBid fBiddingData.biddingWinner fBiddingData.winningBid

          _ ->
            NoOp

      _ ->
        NoOp
  )




port messageReceiver : (String -> msg) -> Sub msg