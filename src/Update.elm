module Update exposing (..)


import Model exposing (..)
-- import SharedData exposing (SentData(..))
import Encoders exposing (sendMessage)
import Json.Encode exposing (Value, encode)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateGameName str ->
      case model of
        BeginGamePage playerName _ ->
          (BeginGamePage playerName str, Cmd.none)

        _ ->
          (model, Cmd.none)

    UpdatePlayerName str ->
      case model of
        BeginGamePage _ gameName ->
          (BeginGamePage str gameName, Cmd.none)

        _ ->
          (model, Cmd.none)

    SendGameName ->
      case model of
        BeginGamePage playerName gameName ->
          ( WaitingForPlayers [playerName] gameName
          , IntroData playerName gameName
            |> sendMessage
          )

        _ ->
          (model, Cmd.none)
    
    BidPlus5 ->
      sendIncreasedBidMessage model 5

    BidPlus10 ->
      sendIncreasedBidMessage model 10

    QuitBidding ->
      case model of
        BiddingRound biddingRoundData ->
          ( { biddingRoundData
            | amIBidding = False
            } |> BiddingRound
          , SendQuit biddingRoundData.gameName biddingRoundData.myIndex
            |> sendMessage
          )

        _ ->
          (model, Cmd.none)

    ReceivedMessageType receivedMessage ->
      handleReceivedMessages receivedMessage model

    -- SentMessageType sentMessage ->
    --   handleSentMessages sentMessage model

    -- TODO: Remove this
    _ ->
      (model, Cmd.none)


handleReceivedMessages : ReceivedMessage -> Model -> (Model, Cmd Msg)
handleReceivedMessages receivedMessage model =
  case receivedMessage of
    PlayerJoined newPlayer ->
      case model of
        WaitingForPlayers players gameName ->
          ( WaitingForPlayers (players ++ [newPlayer]) gameName
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    ExistingPlayers existingPlayers ->
      case model of
        WaitingForPlayers players gameName ->
            ( WaitingForPlayers (existingPlayers ++ players) gameName
            , Cmd.none
            )

        _ ->
          (model, Cmd.none)

    GameData playerSet firstBidder myIndex myCards ->
      case model of
        WaitingForPlayers players gameName ->
          let
            biddingRoundData =
              { playerSet = playerSet
              , highestBid = 150
              , highestBidder = firstBidder
              , bidders = allPlayerIndices
              , amIBidding = True
              , gameName = gameName
              , myIndex = myIndex
              , myCards = myCards
              }
          in
          (BiddingRound biddingRoundData, Cmd.none)

        _ ->
          (model, Cmd.none)

    MaximumBid bidder bid ->
      case model of
        BiddingRound biddingRoundData ->
          ( { biddingRoundData
            | highestBid = bid
            , highestBidder = bidder
            }
            |> BiddingRound
          , Cmd.none
          )
        
        _ ->
          (model, Cmd.none)

    HasQuitBidding quitter ->
      case model of
        BiddingRound biddingRoundData ->
          let
            newBidders = List.filter ((/=) quitter) biddingRoundData.bidders
          in
            -- Is bidding over?
          if List.length newBidders == 0
            -- Did I win bidding?
            then
              if biddingRoundData.myIndex == biddingRoundData.highestBidder
                then
                  ( TrumpSelection
                    { trump = Spade
                    , helper1 = Nothing
                    , helper2 = Nothing
                    , bid = biddingRoundData.highestBid
                    , playerSet = biddingRoundData.playerSet
                    , myIndex = biddingRoundData.myIndex
                    , gameName = biddingRoundData.gameName
                    , myCards = biddingRoundData.myCards
                    }
                  , Cmd.none
                  )
                else
                  ( WaitingForTrump biddingRoundData
                  , Cmd.none
                  )
            else
              ( { biddingRoundData
                | bidders = newBidders
                }
                |> BiddingRound
              , Cmd.none
              )
        
        _ ->
          (model, Cmd.none)



sendIncreasedBidMessage : Model -> Int -> (Model, Cmd Msg)
sendIncreasedBidMessage model delta =
  case model of
    BiddingRound biddingRoundData ->
      let
        newBid = biddingRoundData.highestBid + delta
      in
      ( if newBid >= 250
          then BiddingRound
            { biddingRoundData
            | amIBidding = False
            }
          else model
      , IncreaseBid biddingRoundData.gameName biddingRoundData.myIndex newBid
        |> sendMessage
      )

    _ ->
      (model, Cmd.none)

-- handleSentMessages : SentMessage -> Model -> (Model, Cmd Msg)
-- handleSentMessages sentMessage model =
--   case sentMessage of
--     IntroData playerName gameName ->
--       sendMessage 


--     BeginGame initGameState ->
--       ( initBiddingData initGameState.firstBidder
--         |> \bd -> BiddingRound initGameState allPlayerIndices bd True
--       , Cmd.none
--       )

--     NewHighestBid newHighestBidder newHighestBid ->
--       case model of
--         BiddingRound gameState bidders biddingData isBidding ->
--           if newHighestBid == 0
--             then
--               ( BiddingRound
--                   gameState
--                   (List.filter ((/=) newHighestBidder) bidders)
--                   biddingData
--                   isBidding
--               , Cmd.none
--               )
--             else
--               ( BiddingRound
--                 gameState
--                 bidders
--                 { biddingData
--                 | highestBid = newHighestBid
--                 , highestBidder = newHighestBidder
--                 }
--                 isBidding
--               , Cmd.none
--               )

--         _ ->
--           (model, Cmd.none)

--     FinalBid fBiddingData gameState ->
--       if fBiddingData.biddingWinner == gameState.myIndex
--         then
--           (TrumpSelection initSelectionData fBiddingData gameState, Cmd.none)
--         else
--           (WaitingForTrump fBiddingData gameState, Cmd.none)

--     SelectTrump suit ->
--       case model of
--         TrumpSelection selectionData x y ->
--           ( TrumpSelection
--             { selectionData
--             | selectedTrump = suit
--             } x y
--           , Cmd.none
--           )

--         _ ->
--           (model, Cmd.none)

--     SelectHelper card ->
--       case model of
--         TrumpSelection selectionData x y ->
--           case selectionData.helper1 of
--             Just c1 ->
--               if c1 == card
--                 then
--                   ( TrumpSelection { selectionData | helper1 = Nothing } x y
--                   , Cmd.none
--                   )
--                 else
--                   case selectionData.helper2 of
--                     Just c2 ->
--                       if c2 == card
--                         then
--                           ( TrumpSelection { selectionData | helper2 = Nothing } x y
--                           , Cmd.none
--                           )
--                         else
--                           (model, Cmd.none)

--                     Nothing ->
--                       ( TrumpSelection { selectionData | helper2 = Just card } x y
--                       , Cmd.none
--                       )

--             Nothing ->
--               case selectionData.helper2 of
--                 Just c2 ->
--                   if c2 == card
--                     then
--                       ( TrumpSelection { selectionData | helper2 = Nothing } x y
--                       , Cmd.none
--                       )
--                     else
--                       ( TrumpSelection { selectionData | helper1 = Just card } x y
--                       , Cmd.none
--                       )

--                 Nothing ->
--                   ( TrumpSelection { selectionData | helper1 = Just card } x y
--                   , Cmd.none
--                   )

--         _ ->
--           (model, Cmd.none)

--     SendTrump ->
--       case model of
--         TrumpSelection selectionData fBiddingData gameState ->
--           ( model
--           , encodeSelectionData gameState.gameName selectionData
--             |> sendEncodedValue
--           )

--         _ ->
--           (model, Cmd.none)

--     StartGameplay playState ->
--       (PlayRound Round1 playState True, Cmd.none)

--     SendCard card ->
--       case model of
--         PlayRound round playState _ ->
--           ( PlayRound round playState False
--           , encodePlayedCard playState.gameState.gameName card
--             |> sendEncodedValue
--           )

--         _ ->
--           (model, Cmd.none)

--     PlayCard card nextTurn ->
--       case model of
--         PlayRound round playState _ ->
--           let
--             updateGameState gameState =
--               { gameState
--               | myCards = List.filter ((==) card >> not) gameState.myCards
--               }

--             hadTeamBeenRevealed = playState.helpersRevealed == maxHelpers playState.selectionData

--             updatePlayerStatus oldStatus =
--               case playState.turn of
--                 Just turn ->
--                   if turn == playState.gameState.myIndex || hadTeamBeenRevealed
--                     -- It was my own turn, or the team had already been revealed
--                     then (oldStatus, playState.helpersRevealed)
--                     else
--                       -- Is the card a helper card?
--                       -- If so, set its status to bidding team
--                       -- Also, if all bidding team members have been revealed,
--                       -- set the rest of the players as anti-team
--                       if isPlayerHelper card playState.selectionData
--                         then
--                           let
--                             newStatus = setPlayerStatus turn BiddingTeam oldStatus
--                             newHelpersRevealed = playState.helpersRevealed + 1
--                             hasTeamBeenRevealed = newHelpersRevealed == maxHelpers playState.selectionData
--                           in
--                           -- If team was just revealed, mark the anti team
--                           if hasTeamBeenRevealed
--                             then
--                               getPlayerStatuses newStatus
--                               |> List.filter (Tuple.second >> (/=) BiddingTeam)
--                               |> List.map Tuple.first
--                               |> List.foldl (\p pss -> setPlayerStatus p AntiTeam pss) newStatus
--                               |> \s -> Tuple.pair s newHelpersRevealed
--                             else
--                               (newStatus, newHelpersRevealed)
--                         else
--                           (oldStatus, playState.helpersRevealed)

--                 Nothing ->
--                   (oldStatus, playState.helpersRevealed)

--             (newerStatus, newerHelpersRevealed) = updatePlayerStatus playState.playersStatus

--             newHand =
--               Maybe.map (\t -> setCardInHand t card playState.hand) playState.turn
--               |> Maybe.withDefault playState.hand
--           in
--           ( PlayRound
--               round
--               { playState
--               | gameState = updateGameState playState.gameState
--               , hand = newHand
--               , turn = if nextTurn /= playState.firstPlayer then Just nextTurn else Nothing
--               , playersStatus = newerStatus
--               , helpersRevealed = newerHelpersRevealed
--               }
--               (nextTurn /= playState.firstPlayer)
--           , Cmd.none
--           )

--         _ ->
--           (model, Cmd.none)

--     NextRound firstPlayer playerSet ->
--       case model of
--         PlayRound round playState _ ->
--           let
--             newGameState gameState =
--               { gameState
--               | playerSet = playerSet
--               }
--           in
--           ( PlayRound
--             (nextRound round)
--             { playState
--             | gameState = newGameState playState.gameState
--             , hand = emptyHand
--             , firstPlayer = firstPlayer
--             , turn = Just firstPlayer
--             }
--             True
--           , Cmd.none
--           )

--         _ ->
--           (model, Cmd.none)

--     _ ->
--       (model, Cmd.none)
