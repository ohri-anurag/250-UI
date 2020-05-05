port module Update exposing (..)


import Model exposing (..)
import SharedData exposing (encodeBiddingData, encodeIntroData, encodePlayedCard, encodeSelectionData)
import Json.Encode exposing (Value, encode)


port sendMessage : String -> Cmd msg


sendEncodedValue : Value -> Cmd msg
sendEncodedValue val =
  encode 0 val
  |> sendMessage


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
          ( WaitingForPlayers
          , encodeIntroData playerName gameName
            |> sendEncodedValue
          )

        _ ->
          (model, Cmd.none)

    BeginGame initGameState ->
      ( initBiddingData initGameState.firstBidder
        |> \bd -> BiddingRound initGameState bd True
      , Cmd.none
      )

    BidPlus5 ->
      sendIncreasedBidMessage model 5

    BidPlus10 ->
      sendIncreasedBidMessage model 10

    QuitBidding ->
      case model of
        BiddingRound gameState biddingData _ ->
          ( BiddingRound gameState biddingData False
          , encodeBiddingData gameState.gameName gameState.myIndex 0
            |> sendEncodedValue
          )

        _ ->
          (model, Cmd.none)

    NewHighestBid newHighestBidder newHighestBid ->
      case model of
        BiddingRound gameState biddingData isBidding ->
          ( BiddingRound gameState
            { biddingData
            | highestBid = newHighestBid
            , highestBidder = newHighestBidder
            }
            isBidding
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    FinalBid fBiddingData gameState ->
      if fBiddingData.biddingWinner == gameState.myIndex
        then
          (TrumpSelection initSelectionData fBiddingData gameState, Cmd.none)
        else
          (WaitingForTrump fBiddingData gameState, Cmd.none)

    SelectTrump suit ->
      case model of
        TrumpSelection selectionData x y ->
          ( TrumpSelection
            { selectionData
            | selectedTrump = suit
            } x y
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    SelectHelper card ->
      case model of
        TrumpSelection selectionData x y ->
          case selectionData.helper1 of
            Just c1 ->
              if c1 == card
                then
                  ( TrumpSelection { selectionData | helper1 = Nothing } x y
                  , Cmd.none
                  )
                else
                  case selectionData.helper2 of
                    Just c2 ->
                      if c2 == card
                        then
                          ( TrumpSelection { selectionData | helper2 = Nothing } x y
                          , Cmd.none
                          )
                        else
                          (model, Cmd.none)

                    Nothing ->
                      ( TrumpSelection { selectionData | helper2 = Just card } x y
                      , Cmd.none
                      )

            Nothing ->
              case selectionData.helper2 of
                Just c2 ->
                  if c2 == card
                    then
                      ( TrumpSelection { selectionData | helper2 = Nothing } x y
                      , Cmd.none
                      )
                    else
                      ( TrumpSelection { selectionData | helper1 = Just card } x y
                      , Cmd.none
                      )

                Nothing ->
                  ( TrumpSelection { selectionData | helper1 = Just card } x y
                  , Cmd.none
                  )

        _ ->
          (model, Cmd.none)

    SendTrump ->
      case model of
        TrumpSelection selectionData fBiddingData gameState ->
          ( model
          , encodeSelectionData gameState.gameName selectionData
            |> sendEncodedValue
          )

        _ ->
          (model, Cmd.none)

    StartGameplay playState ->
      (PlayRound Round1 playState True, Cmd.none)

    SendCard card ->
      case model of
        PlayRound round playState _ ->
          ( PlayRound round playState False
          , encodePlayedCard playState.gameState.gameName card
            |> sendEncodedValue
          )

        _ ->
          (model, Cmd.none)

    PlayCard card nextTurn ->
      case model of
        PlayRound round playState _ ->
          let
            updateGameState gameState =
              { gameState
              | myCards = List.filter ((==) card >> not) gameState.myCards
              }

            newHelpersRevealed =
              if isPlayerHelper card playState.selectionData
                then
                  playState.helpersRevealed + 1
                else
                  playState.helpersRevealed

            updatePlayerStatus oldStatus =
              if playState.turn == playState.gameState.myIndex
                -- It was my own turn
                then oldStatus
                else
                  -- Is the card a helper card?
                  -- If so, set its status to bidding team
                  -- Also, if all bidding team members have been revealed,
                  -- set the rest of the players as anti-team
                  if isPlayerHelper card playState.selectionData
                    then
                      let
                        --maxSize = biddingTeamSize playState.selectionData
                        newStatus = setPlayerStatus playState.turn BiddingTeam oldStatus
                        --currentSize =
                        --  getPlayerStatuses newStatus
                        --  |> List.filter (Tuple.second >> (==) BiddingTeam)
                        --  |> List.length
                        hasTeamBeenRevealed = newHelpersRevealed == maxHelpers playState.selectionData
                      in
                      if hasTeamBeenRevealed
                        then
                          getPlayerStatuses newStatus
                          |> List.filter (Tuple.second >> (/=) BiddingTeam)
                          |> List.map Tuple.first
                          |> List.foldl (\p pss -> setPlayerStatus p AntiTeam pss) newStatus
                        else
                          newStatus
                    else
                      oldStatus
          in
          ( PlayRound
              round
              { playState
              | gameState = updateGameState playState.gameState
              , hand = setCardInHand playState.turn card playState.hand
              , turn = nextTurn
              , playersStatus = updatePlayerStatus playState.playersStatus
              , helpersRevealed = newHelpersRevealed
              }
              (nextTurn /= playState.firstPlayer)
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    NextRound playerSet ->
      case model of
        PlayRound round playState _ ->
          let
            newGameState gameState =
              { gameState
              | playerSet = playerSet
              }
          in
          ( PlayRound
            (nextRound round)
            { playState
            | gameState = newGameState playState.gameState
            , hand = emptyHand
            }
            True
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

    _ ->
      (model, Cmd.none)

sendIncreasedBidMessage : Model -> Int -> (Model, Cmd Msg)
sendIncreasedBidMessage model delta =
  case model of
    BiddingRound gameState biddingData _ ->
      let
        newBid = biddingData.highestBid + delta
      in
      ( if newBid == 250
          then BiddingRound gameState biddingData False
          else model
      , newBid
        |> encodeBiddingData gameState.gameName gameState.myIndex
        |> sendEncodedValue
      )

    _ ->
      (model, Cmd.none)