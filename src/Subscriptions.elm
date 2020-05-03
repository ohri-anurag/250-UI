port module Subscriptions exposing (..)


import Json.Decode exposing (decodeString)
import Maybe exposing (andThen)
import String exposing (toInt)


import Model exposing (..)
import SharedData exposing (biddingDataDecoder, gameStateDecoder, roundDataDecoder, selectionDataDecoder)

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

      BiddingRound gameState _ _ ->
        case decodeString biddingDataDecoder str of
          Ok biddingData ->
            case biddingData of
              IntermediateBiddingData iBiddingData ->
                NewHighestBid iBiddingData.highestBidder iBiddingData.highestBid

              FinalBiddingData fBiddingData ->
                FinalBid fBiddingData gameState

          _ ->
            NoOp

      WaitingForTrump fBiddingData gameState ->
        case decodeString selectionDataDecoder str of
          Ok selectionData ->
            StartGameplay
              { gameState = gameState
              , biddingData = fBiddingData
              , selectionData = selectionData
              , turn = gameState.firstBidder
              , hand = emptyHand
              }

          _ ->
            NoOp

      TrumpSelection _ fBiddingData gameState ->
        case decodeString selectionDataDecoder str of
          Ok selectionData ->
            StartGameplay
              { gameState = gameState
              , biddingData = fBiddingData
              , selectionData = selectionData
              , turn = gameState.firstBidder
              , hand = emptyHand
              }

          _ ->
            NoOp

      PlayRound round playState _ ->
        case decodeString roundDataDecoder str of
          Ok roundData ->
            case roundData of
              PlayedCardData playedCard ->
                PlayCard playedCard.playedCard playedCard.turn

              RoundFinishData playerSet ->
                NextRound playerSet

          _->
            NoOp

      _ ->
        NoOp
  )




port messageReceiver : (String -> msg) -> Sub msg