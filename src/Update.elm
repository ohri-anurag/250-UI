module Update exposing (..)


import Model exposing (..)
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
        BiddingRound gameName biddingRoundData ->
          ( { biddingRoundData
            | amIBidding = False
            } |> BiddingRound gameName
          , SendQuit gameName biddingRoundData.myIndex
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
              , highestBid = 150
              , highestBidder = firstBidder
              , bidders = allPlayerIndices
              , amIBidding = True
              , myIndex = myIndex
              , myCards = myCards
              , firstBidder = firstBidder
              }
          in
          (BiddingRound gameName biddingRoundData, Cmd.none)

        _ ->
          (model, Cmd.none)

    MaximumBid bidder bid ->
      case model of
        BiddingRound gameName biddingRoundData ->
          ( { biddingRoundData
            | highestBid = bid
            , highestBidder = bidder
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
              if biddingRoundData.myIndex == biddingRoundData.highestBidder
                then
                  ( TrumpSelection gameName
                    { selectionData =
                      { trump = Spade
                      , helpers = []
                      }
                    , bid = biddingRoundData.highestBid
                    , bidder = biddingRoundData.highestBidder
                    , playerSet = biddingRoundData.playerSet
                    , myIndex = biddingRoundData.myIndex
                    , myCards = biddingRoundData.myCards
                    , firstBidder = biddingRoundData.firstBidder
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
                trumpSelectionData.myIndex
                trumpSelectionData.myIndex
                trumpSelectionData.selectionData
                trumpSelectionData.myCards
                initPlayerStatusSet
          in
          ( PlayRound gameName
            { trumpSelectionData = trumpSelectionData
            , firstPlayer = trumpSelectionData.firstBidder
            , roundIndex = Round1
            , hand = emptyHand
            , playersStatus = playersStatus
            , helpersRevealed = helpersRevealed
            , bidder = trumpSelectionData.myIndex
            , turnStatus =
                if trumpSelectionData.firstBidder == trumpSelectionData.myIndex
                  then FirstAndMyTurn
                  else FirstAndNotMyTurn trumpSelectionData.firstBidder
            }
          , Cmd.none
          )

        WaitingForTrump gameName biddingRoundData ->
          let
            (playersStatus, helpersRevealed) =
              getPlayersStatus
                biddingRoundData.myIndex
                biddingRoundData.highestBidder
                selectionData
                biddingRoundData.myCards
                initPlayerStatusSet
          in
          ( PlayRound gameName
            { trumpSelectionData =
              { selectionData = selectionData
              , bid = biddingRoundData.highestBid
              , bidder = biddingRoundData.highestBidder
              , playerSet = biddingRoundData.playerSet
              , myIndex = biddingRoundData.myIndex
              , myCards = biddingRoundData.myCards
              , firstBidder = biddingRoundData.firstBidder
              }
            , firstPlayer = biddingRoundData.firstBidder
            , roundIndex = Round1
            , hand = emptyHand
            , playersStatus = playersStatus
            , helpersRevealed = helpersRevealed
            , bidder = biddingRoundData.myIndex
            , turnStatus =
                if biddingRoundData.firstBidder == biddingRoundData.myIndex
                  then FirstAndMyTurn
                  else FirstAndNotMyTurn biddingRoundData.firstBidder
            }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    PlayCard card ->
      case model of
        PlayRound gameName playRoundData ->
          let
            updateGameState trumpSelectionData =
              { trumpSelectionData
              | myCards = List.filter ((==) card >> not) trumpSelectionData.myCards
              }

            hadTeamBeenRevealed =
              playRoundData.helpersRevealed == maxHelpers playRoundData.trumpSelectionData.selectionData

            myIndex = playRoundData.trumpSelectionData.myIndex

            (newTurnStatus, oldTurn) =
              case playRoundData.turnStatus of
                FirstAndNotMyTurn firstPlayer ->
                  ( if nextTurn firstPlayer == myIndex
                      then NotFirstAndMyTurn card
                      else NotFirstAndNotMyTurn (nextTurn firstPlayer) card
                  , firstPlayer
                  )

                NotFirstAndNotMyTurn player baseCard ->
                  ( if nextTurn player == myIndex
                      then NotFirstAndMyTurn baseCard
                      else if nextTurn player == playRoundData.firstPlayer
                        then RoundFinished
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
              if oldTurn == playRoundData.trumpSelectionData.myIndex || hadTeamBeenRevealed
                -- It was my own turn, or the team had already been revealed
                then (oldStatus, playRoundData.helpersRevealed)
                else
                  -- Is the card a helper card?
                  -- If so, set its status to bidding team
                  -- Also, if all bidding team members have been revealed,
                  -- set the rest of the players as anti-team
                  if isPlayerHelper card playRoundData.trumpSelectionData.selectionData
                    then
                      let
                        newStatus = setPlayerStatus oldTurn BiddingTeam oldStatus
                        newHelpersRevealed = playRoundData.helpersRevealed + 1
                        hasTeamBeenRevealed = newHelpersRevealed == maxHelpers playRoundData.trumpSelectionData.selectionData
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
            | trumpSelectionData = updateGameState playRoundData.trumpSelectionData
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
            updatePlayers trumpSelectionData =
              { trumpSelectionData
              | playerSet = updatePlayer winner(\p -> 
                  { p | gameScore = p.gameScore + score }
                  ) trumpSelectionData.playerSet
              }
            newRound = nextRound playRoundData.roundIndex
          in
          ( PlayRound gameName
            { playRoundData
            | trumpSelectionData = updatePlayers playRoundData.trumpSelectionData
            , turnStatus =
                if newRound == Round1
                  then GameFinished
                  else if winner == playRoundData.trumpSelectionData.myIndex
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
              ) playRoundData.trumpSelectionData.playerSet allPlayerIndices

            updatePlayerSet trumpSelectionData =
              { trumpSelectionData
              | playerSet = updatedPlayerScores
              }
          in
          ( PlayRound gameName
            { playRoundData
            | trumpSelectionData = updatePlayerSet playRoundData.trumpSelectionData
            }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    NewGame cards ->
      case model of
        PlayRound gameName playRoundData ->
          ( BiddingRound gameName
            { playerSet = playRoundData.trumpSelectionData.playerSet
            , highestBid = playRoundData.trumpSelectionData.bid
            , highestBidder = playRoundData.trumpSelectionData.bidder
            , bidders = allPlayerIndices
            , myIndex = playRoundData.trumpSelectionData.myIndex
            , amIBidding = True
            , myCards = cards
            , firstBidder = playRoundData.trumpSelectionData.firstBidder
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
        newBid = biddingRoundData.highestBid + delta
      in
      ( if newBid >= 250
          then BiddingRound gameName
            { biddingRoundData
            | amIBidding = False
            }
          else model
      , IncreaseBid gameName biddingRoundData.myIndex newBid
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
