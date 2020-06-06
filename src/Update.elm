module Update exposing (..)


import Model.Card exposing (..)
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
        BiddingRound commonData bidders ->
          ( List.filter ((/=) commonData.myData.myIndex) bidders
            |> BiddingRound commonData
          , SendQuit commonData.gameName commonData.myData.myIndex
            |> sendMessage
          )

        _ ->
          (model, Cmd.none)

    SelectTrump suit ->
      case model of
        TrumpSelection commonData selectionData ->
          ( TrumpSelection commonData
              { selectionData
              | trump = suit
              }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    SelectHelper card ->
      case model of
        TrumpSelection commonData selectionData ->
          let
            newSelectionData =
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
          ( TrumpSelection commonData newSelectionData
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    SendTrump ->
      case model of
        TrumpSelection commonData selectionData ->
          ( model
          , SentSelectionData commonData.gameName selectionData
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
        PlayRound commonData playRoundData ->
          ( PlayRound commonData
            { playRoundData | turnStatus = updateTurn playRoundData.turnStatus }
          , PlayedCard commonData.gameName card
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
            commonData =
              { gameName = gameName
              , playerSet = playerSet
              , biddingData =
                { highestBid = 150
                , highestBidder = firstBidder
                , firstBidder = firstBidder
                }
              , myData =
                { myIndex = myIndex
                , myCards = myCards
                }
              }
          in
          ( BiddingRound commonData allPlayerIndices
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    MaximumBid bidder bid ->
      case model of
        BiddingRound commonData bidders ->
          let
            updateBiddingData biddingData =
              { biddingData
              | highestBid = bid
              , highestBidder = bidder
              }
          in
          ( BiddingRound 
              { commonData
              | biddingData = updateBiddingData commonData.biddingData
              }
              bidders
          , Cmd.none
          )
        
        _ ->
          (model, Cmd.none)

    HasQuitBidding quitter ->
      case model of
        BiddingRound commonData bidders ->
          let
            newBidders = List.filter ((/=) quitter) bidders
          in
            -- Is bidding over?
          if List.length newBidders == 0
            -- Did I win bidding?
            then
              if commonData.myData.myIndex == commonData.biddingData.highestBidder
                then
                  ( TrumpSelection commonData
                      { trump = Spade
                      , helpers = []
                      }
                  , Cmd.none
                  )
                else
                  ( WaitingForTrump commonData
                  , Cmd.none
                  )
            else
              ( BiddingRound commonData newBidders
              , Cmd.none
              )
        
        _ ->
          (model, Cmd.none)

    ReceivedSelectionData selectionData ->
      case model of
        TrumpSelection commonData _ ->
          let
            (playersStatus, helpersRevealed) =
              getPlayersStatus
                commonData.myData.myIndex
                commonData.myData.myIndex
                selectionData
                commonData.myData.myCards
                initPlayerStatusSet
            firstBidder = commonData.biddingData.firstBidder
          in
          ( PlayRound commonData
            { selectionData = selectionData
            , firstPlayer = firstBidder
            , roundIndex = Round1
            , hand = emptyHand
            , playersStatus = playersStatus
            , helpersRevealed = helpersRevealed
            , turnStatus =
                if firstBidder == commonData.myData.myIndex
                  then FirstAndMyTurn
                  else FirstAndNotMyTurn firstBidder
            }
          , Cmd.none
          )

        WaitingForTrump commonData ->
          let
            (playersStatus, helpersRevealed) =
              getPlayersStatus
                commonData.myData.myIndex
                commonData.biddingData.highestBidder
                selectionData
                commonData.myData.myCards
                initPlayerStatusSet
            firstBidder = commonData.biddingData.firstBidder
          in
          ( PlayRound commonData
            { selectionData = selectionData
            , firstPlayer = firstBidder
            , roundIndex = Round1
            , hand = emptyHand
            , playersStatus = playersStatus
            , helpersRevealed = helpersRevealed
            , turnStatus =
                if firstBidder == commonData.myData.myIndex
                  then FirstAndMyTurn
                  else FirstAndNotMyTurn firstBidder
            }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    PlayCard card ->
      case model of
        PlayRound commonData playRoundData ->
          let
            updateMyData myData =
              { myData
              | myCards = List.filter ((==) card >> not) myData.myCards
              }

            hadTeamBeenRevealed =
              playRoundData.helpersRevealed == maxHelpers playRoundData.selectionData

            myIndex = commonData.myData.myIndex

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
              if oldTurn == commonData.myData.myIndex || hadTeamBeenRevealed
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
          ( PlayRound 
              { commonData
              | myData = updateMyData commonData.myData
              }
              { playRoundData
              | playersStatus = newerStatus
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
        PlayRound commonData playRoundData ->
          let
            updatePlayers = updatePlayer winner(\p -> 
                  { p | gameScore = p.gameScore + score }
                  ) commonData.playerSet
            newRound = nextRound playRoundData.roundIndex
          in
          ( PlayRound
              { commonData
              | playerSet = updatePlayers
              }
              { playRoundData
              | turnStatus =
                  if newRound == Round1
                    then GameFinished
                    else if winner == commonData.myData.myIndex
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
        PlayRound commonData playRoundData ->
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
              ) commonData.playerSet allPlayerIndices
          in
          ( PlayRound 
              { commonData
              | playerSet = updatedPlayerScores
              }
              playRoundData
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    NewGame cards ->
      case model of
        PlayRound commonData playRoundData ->
          let
            nextFirstBidder = nextTurn commonData.biddingData.firstBidder
          in
          ( BiddingRound
              { commonData
              | biddingData =
                  { highestBid = 150
                  , highestBidder = nextFirstBidder
                  , firstBidder = nextFirstBidder
                  }
              , myData =
                { myIndex = commonData.myData.myIndex
                , myCards = cards
                }
              }
              allPlayerIndices
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    BiddingReconnectionData playerSet biddingData myData bidders  ->
      case model of
        WaitingForPlayers _ gameName ->
          (BiddingRound
            { gameName = gameName
            , playerSet = playerSet
            , biddingData = biddingData
            , myData = myData
            }
            bidders
          , Cmd.none)

        _ ->
          (model, Cmd.none)



sendIncreasedBidMessage : Model -> Int -> (Model, Cmd Msg)
sendIncreasedBidMessage model delta =
  case model of
    BiddingRound commonData bidders ->
      let
        newBid = commonData.biddingData.highestBid + delta
      in
      ( if newBid >= 250
          then BiddingRound commonData []
          else model
      , min newBid 250
        |> IncreaseBid commonData.gameName commonData.myData.myIndex
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
