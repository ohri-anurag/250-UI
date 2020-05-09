module Subscriptions exposing (..)


import Json.Decode exposing (decodeString)
import Maybe exposing (andThen)
import String exposing (toInt)


import Model exposing (..)
-- import SharedData exposing (ReceivedData(..))
import Decoders exposing (receiveMessage)


subscriptions : Model -> Sub Msg
subscriptions _ = receiveMessage
  -- receiveData (\eitherReceivedData ->
  --   case eitherReceivedData of
  --     Ok receivedData ->
  --       case receivedData of
  --         PlayerJoined playerName ->
  --           case model of ->
  --             WaitingForPlayers players ->
  --               players ++ [playerName]
  --               |> PlayersUpdated

  --             _ ->
  --               NoOp

  --         ExistingPlayers existingPlayers ->
  --           case model of ->
  --             WaitingForPlayers players ->
  --               existingPlayers ++ players
  --               |> PlayersUpdated

  --             _ ->
  --               NoOp
  --    Error error ->
  -- )

--       BiddingRound gameState _ _ _ ->
--         case decodeString biddingDataDecoder str of
--           Ok biddingData ->
--             case biddingData of
--               IntermediateBiddingData iBiddingData ->
--                 NewHighestBid iBiddingData.highestBidder iBiddingData.highestBid

--               FinalBiddingData fBiddingData ->
--                 FinalBid fBiddingData gameState

--           _ ->
--             NoOp

--       WaitingForTrump fBiddingData gameState ->
--         case decodeString selectionDataDecoder str of
--           Ok selectionData ->
--             let
--               (newStatusSet, newHelpersRevealed) =
--                 getPlayersStatus
--                   gameState.myIndex
--                   fBiddingData.biddingWinner
--                   selectionData
--                   gameState.myCards
--                   initPlayerStatusSet
--             in
--             StartGameplay
--               { gameState = gameState
--               , biddingData = fBiddingData
--               , selectionData = selectionData
--               , firstPlayer = gameState.firstBidder
--               , turn = Just gameState.firstBidder
--               , hand = emptyHand
--               , playersStatus = newStatusSet
--               , helpersRevealed = newHelpersRevealed
--               }

--           _ ->
--             NoOp

--       TrumpSelection _ fBiddingData gameState ->
--         case decodeString selectionDataDecoder str of
--           Ok selectionData ->
--             let
--               (newStatusSet, newHelpersRevealed) =
--                 getPlayersStatus
--                   gameState.myIndex
--                   fBiddingData.biddingWinner
--                   selectionData
--                   gameState.myCards
--                   initPlayerStatusSet
--             in
--             StartGameplay
--               { gameState = gameState
--               , biddingData = fBiddingData
--               , selectionData = selectionData
--               , firstPlayer = gameState.firstBidder
--               , turn = Just gameState.firstBidder
--               , hand = emptyHand
--               , playersStatus = newStatusSet
--               , helpersRevealed = newHelpersRevealed
--               }

--           _ ->
--             NoOp

--       PlayRound round playState _ ->
--         case decodeString roundDataDecoder str of
--           Ok roundData ->
--             case roundData of
--               PlayedCardData playedCard ->
--                 PlayCard playedCard.playedCard playedCard.turn

--               RoundFinishData nextRoundData ->
--                 NextRound nextRoundData.firstPlayer nextRoundData.playerSet

--               GameFinishData nextGameData ->
--                 BeginGame nextGameData


--           _->
--             NoOp

--       _ ->
--         NoOp
--   )


-- getPlayersStatus : PlayerIndex -> PlayerIndex -> SelectionData -> List Card -> PlayerStatusSet -> (PlayerStatusSet, Int)
-- getPlayersStatus myIndex winnerIndex selectionData myCards playerStatusSet =
--   let
--     -- Set the bidder's status to bidding team
--     newStatusSet = setPlayerStatus winnerIndex BiddingTeam playerStatusSet

--     -- Apart from the bidder, all are anti team
--     allAntiStatus =
--       List.filter ((/=) winnerIndex) allPlayerIndices
--       |> List.foldl (\p pss -> setPlayerStatus p AntiTeam pss) newStatusSet
--   in
--   -- The bidder did not ask for any helper, everyone knows the status
--   if maxHelpers selectionData == 0
--     then (allAntiStatus, 0)
--     else if myIndex == winnerIndex
--       then
--         -- My status has already been set, since I am the bidder
--         (newStatusSet, 0)
--       else
--         -- I am not the bidder
--         if amITheOnlyHelper myCards selectionData
--           -- I am the only helper, set all other statuses to AntiTeam
--           then
--             ( setPlayerStatus myIndex BiddingTeam allAntiStatus
--             , maxHelpers selectionData
--             )
--           else if amIHelper myCards selectionData
--             -- There is another helper
--             then
--               (setPlayerStatus myIndex BiddingTeam newStatusSet, 1)
--             else
--               -- I am not a helper
--               (setPlayerStatus myIndex AntiTeam newStatusSet, 0)



-- port messageReceiver : (String -> msg) -> Sub msg