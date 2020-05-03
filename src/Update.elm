port module Update exposing (..)


import Model exposing (..)
import SharedData exposing (encodeBiddingData, encodeIntroData, encodeSelectionData)
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
      let
        setHelper1 selectionData val x y =
          TrumpSelection
            { selectionData
            | helper1 = val
            } x y

        setHelper2 selectionData val x y =
          TrumpSelection
            { selectionData
            | helper2 = val
            } x y

      in

      case model of
        TrumpSelection selectionData x y ->
          case selectionData.helper1 of
            Just c1 ->
              if c1 == card
                then
                  ( setHelper1 selectionData Nothing x y
                  , Cmd.none
                  )
                else
                  case selectionData.helper2 of
                    Just c2 ->
                      if c2 == card
                        then
                          ( setHelper2 selectionData Nothing x y
                          , Cmd.none
                          )
                        else
                          (model, Cmd.none)

                    Nothing ->
                      ( setHelper2 selectionData (Just card) x y
                      , Cmd.none
                      )

            Nothing ->
              ( setHelper1 selectionData (Just card) x y
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

    StartGameplay ->
      (Round1, Cmd.none)

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