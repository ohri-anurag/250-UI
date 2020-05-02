port module Update exposing (..)


import Model exposing (..)
import SharedData exposing (encodeIntroData, encodeBiddingData)


port sendMessage : String -> Cmd msg


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
            |> sendMessage
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
            |> sendMessage
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

    FinalBid _ _ ->
      (Round1, Cmd.none)

    _ ->
      (model, Cmd.none)

sendIncreasedBidMessage : Model -> Int -> (Model, Cmd Msg)
sendIncreasedBidMessage model delta =
  case model of
    BiddingRound gameState biddingData _ ->
      ( model
      , biddingData.highestBid + delta
        |> encodeBiddingData gameState.gameName gameState.myIndex
        |> sendMessage
      )

    _ ->
      (model, Cmd.none)