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
              , playersStatus =
                  getPlayersStatus
                    gameState.myIndex
                    fBiddingData.biddingWinner
                    selectionData
                    gameState.myCards
                    initPlayerStatusSet
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
              , playersStatus =
                  getPlayersStatus
                    gameState.myIndex
                    fBiddingData.biddingWinner
                    selectionData
                    gameState.myCards
                    initPlayerStatusSet
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


getPlayersStatus : PlayerIndex -> PlayerIndex -> SelectionData -> List Card -> PlayerStatusSet -> PlayerStatusSet
getPlayersStatus myIndex winnerIndex selectionData myCards playerStatusSet =
  let
    onlyBidderInBiddingTeam statusSet =
      -- What if bidder doesn't ask for any helper
      if biddingTeamSize selectionData == 1
        then
          List.filter ((/=) myIndex) allPlayerIndices
          |> List.foldl (\p pss -> setPlayerStatus p AntiTeam pss) statusSet
        else
          -- Bidder asked for at least one helper, so return the set as is.
          statusSet

    -- Set the bidder's status to bidding team
    newStatusSet = setPlayerStatus winnerIndex BiddingTeam playerStatusSet
  in
  if myIndex == winnerIndex
    then
      -- My status has already been set
      onlyBidderInBiddingTeam newStatusSet
    else
      -- I am not the bidder
      -- Setting my own status
      if amIHelper myCards selectionData
        then
          setPlayerStatus myIndex BiddingTeam newStatusSet
        else
          setPlayerStatus myIndex AntiTeam newStatusSet


port messageReceiver : (String -> msg) -> Sub msg