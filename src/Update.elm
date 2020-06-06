module Update exposing (..)


import Model exposing (..)
import Encoders exposing (sendMessage)
import Json.Encode exposing (Value, encode)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateGameName str ->
      case model of
        BeginGamePage playerId playerName _ ->
          (BeginGamePage playerId playerName str, Cmd.none)

        _ ->
          (model, Cmd.none)

    UpdatePlayerName str ->
      case model of
        BeginGamePage playerId _ gameName ->
          (BeginGamePage playerId str gameName, Cmd.none)

        _ ->
          (model, Cmd.none)

    UpdatePlayerId str ->
      case model of
        BeginGamePage _ playerName gameName ->
          (BeginGamePage str playerName gameName, Cmd.none)

        _ ->
          (model, Cmd.none)

    SendGameName ->
      case model of
        BeginGamePage playerId playerName gameName ->
          ( WaitingForPlayers [playerName] gameName
          , IntroData playerId playerName gameName
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
        BiddingRound gameName biddingRoundData ->
          ( { biddingRoundData
            | amIBidding = False
            } |> BiddingRound gameName
          , SendQuit gameName biddingRoundData.myData.myIndex
            |> sendMessage
          )

        _ ->
          (model, Cmd.none)

    SelectTrump suit ->
      let
        updateTrump selectionData = { selectionData | trump = suit }
      in
      case model of
        TrumpSelection gameName trumpSelectionData ->
          ( TrumpSelection gameName
            { trumpSelectionData
            | selectionData = updateTrump trumpSelectionData.selectionData
            }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    SelectHelper card ->
      case model of
        TrumpSelection gameName trumpSelectionData ->
          let
            updateHelpers selectionData =
              -- Was card already a helper?
              if List.member card selectionData.helpers
                then
                  { selectionData
                  | helpers = List.filter ((/=) card) selectionData.helpers
                  }
                -- If not, can I add it to current helpers?
                else if List.length selectionData.helpers < 2
                  then
                    { selectionData
                    | helpers = card :: selectionData.helpers
                    }
                  -- Already have 2 helpers, can't have more
                  else selectionData
          in
          ( TrumpSelection gameName
            { trumpSelectionData
            | selectionData = updateHelpers trumpSelectionData.selectionData
            }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    SendTrump ->
      case model of
        TrumpSelection gameName trumpSelectionData ->
          ( model
          , SentSelectionData gameName trumpSelectionData.selectionData
            |> sendMessage
          )

        _ ->
          (model, Cmd.none)

    SendCard card ->
      let
        updateTurn turnStatus =
          case turnStatus of
            FirstAndMyTurn ->
              FirstAndMyTurnOver
            NotFirstAndMyTurn baseCard ->
              NotFirstAndMyTurnOver baseCard
            _ ->
              turnStatus
      in
      case model of
        PlayRound gameName playRoundData ->
          ( PlayRound gameName
            { playRoundData | turnStatus = updateTurn playRoundData.turnStatus }
          , PlayedCard gameName card
            |> sendMessage
          )

        _ ->
          (model, Cmd.none)

    ReceivedMessageType receivedMessage ->
      handleReceivedMessages receivedMessage model

    SentMessageType _ ->
      (model, Cmd.none)

    NoOp ->
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
              , biddingData =
                { highestBid = 150
                , highestBidder = firstBidder
                , firstBidder = firstBidder
                }
              , bidders = allPlayerIndices
              , amIBidding = True
              , myData =
                { myIndex = myIndex
                , myCards = myCards
                }
              }
          in
          (BiddingRound gameName biddingRoundData, Cmd.none)

        _ ->
          (model, Cmd.none)

    MaximumBid bidder bid ->
      case model of
        BiddingRound gameName biddingRoundData ->
          let
            updateBiddingData biddingData =
              { biddingData
              | highestBid = bid
              , highestBidder = bidder
              }
          in
          ( { biddingRoundData
            | biddingData = updateBiddingData biddingRoundData.biddingData
            }
            |> BiddingRound gameName
          , Cmd.none
          )
        
        _ ->
          (model, Cmd.none)

    HasQuitBidding quitter ->
      case model of
        BiddingRound gameName biddingRoundData ->
          let
            newBidders = List.filter ((/=) quitter) biddingRoundData.bidders
          in
            -- Is bidding over?
          if List.length newBidders == 0
            -- Did I win bidding?
            then
              if biddingRoundData.myData.myIndex == biddingRoundData.biddingData.highestBidder
                then
                  ( TrumpSelection gameName
                    { selectionData =
                      { trump = Spade
                      , helpers = []
                      }
                    , biddingData = biddingRoundData.biddingData
                    , playerSet = biddingRoundData.playerSet
                    , myData = biddingRoundData.myData
                    }
                  , Cmd.none
                  )
                else
                  ( WaitingForTrump gameName biddingRoundData
                  , Cmd.none
                  )
            else
              ( { biddingRoundData
                | bidders = newBidders
                }
                |> BiddingRound gameName
              , Cmd.none
              )
        
        _ ->
          (model, Cmd.none)

    ReceivedSelectionData selectionData ->
      case model of
        TrumpSelection gameName trumpSelectionData ->
          let
            (playersStatus, helpersRevealed) =
              getPlayersStatus
                trumpSelectionData.myData.myIndex
                trumpSelectionData.myData.myIndex
                trumpSelectionData.selectionData
                trumpSelectionData.myData.myCards
                initPlayerStatusSet
            firstBidder = trumpSelectionData.biddingData.firstBidder
          in
          ( PlayRound gameName
            { selectionData = trumpSelectionData.selectionData
            , biddingData = trumpSelectionData.biddingData
            , playerSet = trumpSelectionData.playerSet
            , myData = trumpSelectionData.myData
            , firstPlayer = firstBidder
            , roundIndex = Round1
            , hand = emptyHand
            , playersStatus = playersStatus
            , helpersRevealed = helpersRevealed
            , turnStatus =
                if firstBidder == trumpSelectionData.myData.myIndex
                  then FirstAndMyTurn
                  else FirstAndNotMyTurn firstBidder
            }
          , Cmd.none
          )

        WaitingForTrump gameName biddingRoundData ->
          let
            (playersStatus, helpersRevealed) =
              getPlayersStatus
                biddingRoundData.myData.myIndex
                biddingRoundData.biddingData.highestBidder
                selectionData
                biddingRoundData.myData.myCards
                initPlayerStatusSet
            firstBidder = biddingRoundData.biddingData.firstBidder
          in
          ( PlayRound gameName
            { selectionData = selectionData
            , biddingData = biddingRoundData.biddingData
            , playerSet = biddingRoundData.playerSet
            , myData = biddingRoundData.myData
            , firstPlayer = firstBidder
            , roundIndex = Round1
            , hand = emptyHand
            , playersStatus = playersStatus
            , helpersRevealed = helpersRevealed
            , turnStatus =
                if firstBidder == biddingRoundData.myData.myIndex
                  then FirstAndMyTurn
                  else FirstAndNotMyTurn firstBidder
            }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    PlayCard card ->
      case model of
        PlayRound gameName playRoundData ->
          let
            updateMyData myData =
              { myData
              | myCards = List.filter ((==) card >> not) myData.myCards
              }

            hadTeamBeenRevealed =
              playRoundData.helpersRevealed == maxHelpers playRoundData.selectionData

            myIndex = playRoundData.myData.myIndex

            (newTurnStatus, oldTurn) =
              case playRoundData.turnStatus of
                FirstAndNotMyTurn firstPlayer ->
                  ( if nextTurn firstPlayer == myIndex
                      then NotFirstAndMyTurn card
                      else NotFirstAndNotMyTurn (nextTurn firstPlayer) card
                  , firstPlayer
                  )

                NotFirstAndNotMyTurn player baseCard ->
                  ( if nextTurn player == playRoundData.firstPlayer
                      then RoundFinished
                      else if nextTurn player == myIndex
                        then NotFirstAndMyTurn baseCard
                        else NotFirstAndNotMyTurn (nextTurn player) baseCard
                  , player
                  )

                FirstAndMyTurnOver ->
                  ( NotFirstAndNotMyTurn (nextTurn myIndex) card
                  , myIndex
                  )

                NotFirstAndMyTurnOver baseCard ->
                  ( if nextTurn myIndex == playRoundData.firstPlayer
                      then RoundFinished
                      else NotFirstAndNotMyTurn (nextTurn myIndex) card
                  , myIndex
                  )

                -- Unreachable case
                _ -> (playRoundData.turnStatus, Player1)

            updatePlayerStatus oldStatus =
              if oldTurn == playRoundData.myData.myIndex || hadTeamBeenRevealed
                -- It was my own turn, or the team had already been revealed
                then (oldStatus, playRoundData.helpersRevealed)
                else
                  -- Is the card a helper card?
                  -- If so, set its status to bidding team
                  -- Also, if all bidding team members have been revealed,
                  -- set the rest of the players as anti-team
                  if isPlayerHelper card playRoundData.selectionData
                    then
                      let
                        newStatus = setPlayerStatus oldTurn BiddingTeam oldStatus
                        newHelpersRevealed = playRoundData.helpersRevealed + 1
                        hasTeamBeenRevealed = newHelpersRevealed == maxHelpers playRoundData.selectionData
                      in
                      -- If team was just revealed, mark the anti team
                      if hasTeamBeenRevealed
                        then
                          getPlayerStatuses newStatus
                          |> List.filter (Tuple.second >> (/=) BiddingTeam)
                          |> List.map Tuple.first
                          |> List.foldl (\p pss -> setPlayerStatus p AntiTeam pss) newStatus
                          |> \s -> Tuple.pair s newHelpersRevealed
                        else
                          (newStatus, newHelpersRevealed)
                    else
                      (oldStatus, playRoundData.helpersRevealed)

            (newerStatus, newerHelpersRevealed) = updatePlayerStatus playRoundData.playersStatus

            newHand = setCardInHand oldTurn card playRoundData.hand
          in
          ( PlayRound gameName
            { playRoundData
            | myData = updateMyData playRoundData.myData
            , playersStatus = newerStatus
            , helpersRevealed = newerHelpersRevealed
            , turnStatus = newTurnStatus
            , hand = newHand
            }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    RoundData winner score ->
      case model of
        PlayRound gameName playRoundData ->
          let
            updatePlayers = updatePlayer winner(\p -> 
                  { p | gameScore = p.gameScore + score }
                  ) playRoundData.playerSet
            newRound = nextRound playRoundData.roundIndex
          in
          ( PlayRound gameName
            { playRoundData
            | playerSet = updatePlayers
            , turnStatus =
                if newRound == Round1
                  then GameFinished
                  else if winner == playRoundData.myData.myIndex
                    then FirstAndMyTurn
                    else FirstAndNotMyTurn winner
            , roundIndex = newRound
            , hand = emptyHand
            , firstPlayer = winner
            }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    GameFinishedData winningTeam totalScore ->
      case model of
        PlayRound gameName playRoundData ->
          let
            updatedPlayerScores =
              List.foldl (\playerIndex playerSet ->
                updatePlayer playerIndex (\player ->
                  { player
                  | gameScore = 0
                  , totalScore =
                      player.totalScore +
                      if List.member playerIndex winningTeam
                        then totalScore
                        else 0
                  }
                ) playerSet
              ) playRoundData.playerSet allPlayerIndices
          in
          ( PlayRound gameName
            { playRoundData
            | playerSet = updatedPlayerScores
            }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    NewGame cards ->
      case model of
        PlayRound gameName playRoundData ->
          let
            nextFirstBidder = nextTurn playRoundData.biddingData.firstBidder
          in
          ( BiddingRound gameName
            { playerSet = playRoundData.playerSet
            , biddingData =
              { highestBid = 150
              , highestBidder = nextFirstBidder
              , firstBidder = nextFirstBidder
              }
            , bidders = allPlayerIndices
            , myData =
              { myIndex = playRoundData.myData.myIndex
              , myCards = cards
              }
            , amIBidding = True
            }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)


sendIncreasedBidMessage : Model -> Int -> (Model, Cmd Msg)
sendIncreasedBidMessage model delta =
  case model of
    BiddingRound gameName biddingRoundData ->
      let
        newBid = biddingRoundData.biddingData.highestBid + delta
      in
      ( if newBid >= 250
          then BiddingRound gameName
            { biddingRoundData
            | amIBidding = False
            }
          else model
      , min newBid 250
        |> IncreaseBid gameName biddingRoundData.myData.myIndex
        |> sendMessage
      )

    _ ->
      (model, Cmd.none)


getPlayersStatus : PlayerIndex -> PlayerIndex -> SelectionData -> List Card -> PlayerStatusSet -> (PlayerStatusSet, Int)
getPlayersStatus myIndex winnerIndex selectionData myCards playerStatusSet =
  let
    -- Set the bidder's status to bidding team
    newStatusSet = setPlayerStatus winnerIndex BiddingTeam playerStatusSet

    -- Apart from the bidder, all are anti team
    allAntiStatus =
      List.filter ((/=) winnerIndex) allPlayerIndices
      |> List.foldl (\p pss -> setPlayerStatus p AntiTeam pss) newStatusSet
  in
  -- The bidder did not ask for any helper, everyone knows the status
  if maxHelpers selectionData == 0
    then (allAntiStatus, 0)
    else if myIndex == winnerIndex
      then
        -- My status has already been set, since I am the bidder
        (newStatusSet, 0)
      else
        -- I am not the bidder
        if amITheOnlyHelper myCards selectionData
          -- I am the only helper, set all other statuses to AntiTeam
          then
            ( setPlayerStatus myIndex BiddingTeam allAntiStatus
            , maxHelpers selectionData
            )
          else if amIHelper myCards selectionData
            -- There is another helper
            then
              (setPlayerStatus myIndex BiddingTeam newStatusSet, 1)
            else
              -- I am not a helper
              (setPlayerStatus myIndex AntiTeam newStatusSet, 0)
