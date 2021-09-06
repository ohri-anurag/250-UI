module Update exposing (..)


import Http


import Decoders exposing (totalDataDecoder)
import Encoders exposing (sendMessage)
import Json.Encode exposing (Value, encode)
import Model exposing (..)
import Model.Analytics exposing (..)
import Model.Card exposing (..)
import Update.Analytics exposing (..)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateGameName str ->
      case model of
        BeginGamePage playerId playerName _ validation ->
          (BeginGamePage playerId playerName str validation, Cmd.none)

        _ ->
          (model, Cmd.none)

    UpdatePlayerName str ->
      case model of
        BeginGamePage playerId _ gameName validation ->
          (BeginGamePage playerId str gameName validation, Cmd.none)

        _ ->
          (model, Cmd.none)

    UpdatePlayerId str ->
      case model of
        BeginGamePage _ playerName gameName validation ->
          (BeginGamePage str playerName gameName validation, Cmd.none)

        _ ->
          (model, Cmd.none)

    AnalyticsClicked ->
      let
        errorHandler result =
          case result of
            Ok val -> HttpDataType <| TotalDataReceived val
            Err err -> NoOp
      in
      case model of
        BeginGamePage playerId playerName gameName validation ->
          ( BeginGamePage playerId playerName gameName validation
          , Http.get
            -- { url = "http://localhost:8080/total"
            { url = "/total"
            , expect = Http.expectJson errorHandler totalDataDecoder
            }
          )

        _ ->
          (model, Cmd.none)

    SendGameName ->
      case model of
        BeginGamePage playerId playerName gameName _ ->
          let
            validation =
              if playerId == "" then
                Just EmptyId
              else if playerName == "" then
                Just EmptyName
              else if gameName == "" then
                Just EmptyGameName
              else Nothing
          in
          case validation of
            Nothing ->
              ( WaitingForServerValidation playerId playerName gameName
              , IntroData playerId playerName gameName
                |> sendMessage
              )

            v ->
              ( BeginGamePage playerId playerName gameName v
              , Cmd.none
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

    ConfirmTrump ->
      case model of
        TrumpSelection commonData selectionData ->
          ( TrumpConfirmation commonData selectionData
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    SendTrump ->
      case model of
        TrumpConfirmation commonData selectionData ->
          ( model
          , SentSelectionData commonData.gameName selectionData
            |> sendMessage
          )

        _ ->
          (model, Cmd.none)

    DeclineTrump ->
      case model of
        TrumpConfirmation commonData selectionData ->
          ( TrumpSelection commonData selectionData
          , Cmd.none
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

    HttpDataType httpData ->
      handleHttpData httpData model

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
        WaitingForServerValidation playerId playerName gameName ->
          ( WaitingForPlayers (existingPlayers ++ [playerName]) gameName
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
        TrumpConfirmation commonData _ ->
          let
            (playerSet, helpersRevealed) =
              getPlayersStatus
                commonData.myData
                commonData.myData.myIndex
                selectionData
                commonData.playerSet
            firstBidder = commonData.biddingData.firstBidder
          in
          ( PlayRound
              { commonData
              | playerSet = playerSet
              }
              { selectionData = selectionData
              , firstPlayer = firstBidder
              , roundIndex = Round1
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
            (playerSet, helpersRevealed) =
              getPlayersStatus
                commonData.myData
                commonData.biddingData.highestBidder
                selectionData
                commonData.playerSet
            firstBidder = commonData.biddingData.firstBidder
          in
          ( PlayRound
              { commonData
              | playerSet = playerSet
              }
              { selectionData = selectionData
              , firstPlayer = firstBidder
              , roundIndex = Round1
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
              | myCards = List.filter ((/=) card) myData.myCards
              }

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
                      else NotFirstAndNotMyTurn (nextTurn myIndex) baseCard
                  , myIndex
                  )

                -- Unreachable case
                _ -> (playRoundData.turnStatus, Player1)

            hadTeamBeenRevealed =
              playRoundData.helpersRevealed == maxHelpers playRoundData.selectionData

            updatePlayerSet oldSet =
              if oldTurn == commonData.myData.myIndex || hadTeamBeenRevealed
                -- It was my own turn, or the team had already been revealed
                then (oldSet, playRoundData.helpersRevealed)
                else
                  -- Is the card a helper card?
                  -- If so, set its status to bidding team
                  -- Also, if all bidding team members have been revealed,
                  -- set the rest of the players as anti-team
                  if isPlayerHelper card playRoundData.selectionData
                    then
                      let
                        newSet = updatePlayerStatus oldTurn BiddingTeam oldSet
                        newHelpersRevealed = playRoundData.helpersRevealed + 1
                        hasTeamBeenRevealed = newHelpersRevealed == maxHelpers playRoundData.selectionData
                      in
                      -- If team was just revealed, mark the anti team
                      if hasTeamBeenRevealed
                        then
                          getPlayers .status newSet
                          |> List.filter (Tuple.second >> (/=) BiddingTeam)
                          |> List.map Tuple.first
                          |> List.foldl (\p pss -> updatePlayerStatus p AntiTeam pss) newSet
                          |> \s -> Tuple.pair s newHelpersRevealed
                        else
                          (newSet, newHelpersRevealed)
                    else
                      (oldSet, playRoundData.helpersRevealed)

            (newerSet, newerHelpersRevealed) = updatePlayerSet commonData.playerSet
          in
          ( PlayRound
              { commonData
              | myData = updateMyData commonData.myData
              , playerSet = updateCardInSet oldTurn card newerSet
              }
              { playRoundData
              | helpersRevealed = newerHelpersRevealed
              , turnStatus = newTurnStatus
              }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    RoundData winner score ->
      case model of
        PlayRound commonData playRoundData ->
          let
            newPlayerSet =
              List.foldr (\playerIndex playerSet ->
                updatePlayer playerIndex (\player ->
                  { player
                  | gameScore = if playerIndex == winner then player.gameScore + score else player.gameScore
                  , card = Nothing
                  }
                  ) playerSet
              ) commonData.playerSet allPlayerIndices
            newRound = nextRound playRoundData.roundIndex
          in
          ( PlayRound
              { commonData
              | playerSet = newPlayerSet
              }
              { playRoundData
              | turnStatus =
                  if newRound == Round1
                    then GameFinished
                    else if winner == commonData.myData.myIndex
                      then FirstAndMyTurn
                      else FirstAndNotMyTurn winner
              , roundIndex = newRound
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
                  , status = Undecided
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
        WaitingForServerValidation _ _ gameName ->
          -- We are in trump selection state
          if List.length bidders == 0
            -- I am the bidder
            then if biddingData.highestBidder == myData.myIndex
              then
                ( TrumpSelection
                    { gameName = gameName
                    , playerSet = playerSet
                    , biddingData = biddingData
                    , myData = myData
                    }
                    { trump = Spade
                    , helpers = []
                    }
                , Cmd.none)
              else
                ( WaitingForTrump
                    { gameName = gameName
                    , playerSet = playerSet
                    , biddingData = biddingData
                    , myData = myData
                    }
                , Cmd.none
                )
            else
              ( BiddingRound
                  { gameName = gameName
                  , playerSet = playerSet
                  , biddingData = biddingData
                  , myData = myData
                  }
                  bidders
              , Cmd.none)

        _ ->
          (model, Cmd.none)

    RoundReconnectionData playerSet biddingData myData selectionData firstPlayer turn round ->
      case model of
        WaitingForServerValidation _ _ gameName ->
          (PlayRound
            { gameName = gameName
            , playerSet = playerSet
            , biddingData = biddingData
            , myData = myData
            }
            { selectionData = selectionData
            , firstPlayer = firstPlayer
            , roundIndex = round
            , helpersRevealed = calculateHelpersRevealed playerSet selectionData
            , turnStatus = calculateTurnStatus turn firstPlayer myData.myIndex playerSet
            }
          , Cmd.none)

        _ ->
          (model, Cmd.none)

    WebsocketFailed ->
      (ErrorState, Cmd.none)

    PlayerWithIdAlreadyExists ->
      case model of
        WaitingForServerValidation playerId playerName gameName ->
          ( BeginGamePage playerId playerName gameName <| Just DuplicateId
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    PlayerWithNameAlreadyExists ->
      case model of
        WaitingForServerValidation playerId playerName gameName ->
          ( BeginGamePage playerId playerName gameName <| Just DuplicateName
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)


calculateHelpersRevealed : PlayerSet -> SelectionData -> Int
calculateHelpersRevealed playerSet selectionData =
  let
    players = getPlayers .status playerSet

    hasTeamBeenRevealed = List.all (Tuple.second >> (/=) Undecided) players

    biddingTeam = List.filter (Tuple.second >> (==) BiddingTeam) players
  in
  if hasTeamBeenRevealed
    then List.length selectionData.helpers
    else List.length biddingTeam - 1


calculateTurnStatus : PlayerIndex -> PlayerIndex -> PlayerIndex -> PlayerSet -> TurnStatus
calculateTurnStatus turn firstPlayer myIndex playerSet =
  let
    baseCard =
      getPlayer playerSet firstPlayer
      |> .card
      |> Maybe.withDefault (Card Ace Spade)
  in
  if myIndex == firstPlayer
    then if myIndex == turn
      then FirstAndMyTurn
      else FirstAndNotMyTurn turn
    else if myIndex == turn
      then NotFirstAndMyTurn baseCard
      else NotFirstAndNotMyTurn turn baseCard


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


getPlayersStatus : MyData -> PlayerIndex -> SelectionData -> PlayerSet -> (PlayerSet, Int)
getPlayersStatus myData winnerIndex selectionData playerSet =
  let
    -- Set the bidder's status to bidding team
    newPlayerSet = updatePlayerStatus winnerIndex BiddingTeam playerSet

    -- Apart from the bidder, all are anti team
    allAntiStatus =
      List.filter ((/=) winnerIndex) allPlayerIndices
      |> List.foldl (\p pss -> updatePlayerStatus p AntiTeam pss) newPlayerSet
  in
  -- The bidder did not ask for any helper, everyone knows the status
  if maxHelpers selectionData == 0
    then (allAntiStatus, 0)
    else if myData.myIndex == winnerIndex
      then
        -- My status has already been set, since I am the bidder
        (newPlayerSet, 0)
      else
        -- I am not the bidder
        if amITheOnlyHelper myData.myCards selectionData
          -- I am the only helper, set all other statuses to AntiTeam
          then
            ( updatePlayerStatus myData.myIndex BiddingTeam allAntiStatus
            , maxHelpers selectionData
            )
          else if amIHelper myData.myCards selectionData
            -- There is another helper
            then
              (updatePlayerStatus myData.myIndex BiddingTeam newPlayerSet, 1)
            else
              -- I am not a helper
              (updatePlayerStatus myData.myIndex AntiTeam newPlayerSet, 0)
