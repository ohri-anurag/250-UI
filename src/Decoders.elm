port module Decoders exposing(..)


import Json.Decode exposing (..)


import Model exposing (..)
-- import SharedData exposing (ReceivedData(..))


port messageReceiver : (String -> msg) -> Sub msg


type alias PlayerNameSet =
  { name1 : String
  , name2 : String
  , name3 : String
  , name4 : String
  , name5 : String
  , name6 : String
  }


receiveMessage : Sub Msg
receiveMessage = messageReceiver (\str ->
  case decodeString receivedDataDecoder str of
    Ok receivedMessage ->
      ReceivedMessageType receivedMessage

    Err error ->
      NoOp
  )


receivedDataDecoder : Decoder ReceivedMessage
receivedDataDecoder =
  let
    playerWithName name =
      { name = name
      , gameScore = 0
      , totalScore = 0
      }
    playerNameSetToPlayerSet playerNameSet = succeed
      { player1 = playerWithName playerNameSet.name1
      , player2 = playerWithName playerNameSet.name2
      , player3 = playerWithName playerNameSet.name3
      , player4 = playerWithName playerNameSet.name4
      , player5 = playerWithName playerNameSet.name5
      , player6 = playerWithName playerNameSet.name6
      }
  in
  field "tag" string
  |> andThen (\tag ->
    case tag of
      "PlayerJoined" ->
        field "newPlayer" string
        |> map PlayerJoined

      "ExistingPlayers" ->
        list string
        |> field "existingPlayers"
        |> map ExistingPlayers
      
      "GameData" ->
        map4 GameData
          (field "playerNames" (andThen playerNameSetToPlayerSet playerNameSetDecoder))
          (field "firstBidder" playerIndexDecoder)
          (field "myIndex" playerIndexDecoder)
          (field "myCards" cardsDecoder)

      _ ->
        "Unknown tag received: " ++ tag |> fail
  )


suitDecoder : Decoder Suit
suitDecoder =
  string
  |> andThen (\str ->
    case str of
      "Club" ->
        succeed Club

      "Heart" ->
        succeed Heart

      "Diamond" ->
        succeed Diamond

      "Spade" ->
        succeed Spade

      _ ->
        "Unknown Suit: " ++ str |> fail
  )


valueDecoder : Decoder CardValue
valueDecoder =
  string
  |> andThen (\str ->
    case str of
      "Ace" ->
        succeed Ace

      "Three" ->
        succeed Three

      "Four" ->
        succeed Four

      "Five" ->
        succeed Five

      "Six" ->
        succeed Six

      "Seven" ->
        succeed Seven

      "Eight" ->
        succeed Eight

      "Nine" ->
        succeed Nine

      "Ten" ->
        succeed Ten

      "Jack" ->
        succeed Jack

      "Queen" ->
        succeed Queen

      "King" ->
        succeed King


      _ ->
        "Unknown CardValue: " ++ str |> fail
  )


cardDecoder : Decoder Card
cardDecoder =
  map2 Card (field "value" valueDecoder) (field "suit" suitDecoder)


cardsDecoder : Decoder (List Card)
cardsDecoder =
  list cardDecoder


playerIndexDecoder : Decoder PlayerIndex
playerIndexDecoder =
  string
  |> andThen (\str ->
    case str of
      "Player1" ->
        succeed Player1

      "Player2" ->
        succeed Player2

      "Player3" ->
        succeed Player3

      "Player4" ->
        succeed Player4

      "Player5" ->
        succeed Player5

      "Player6" ->
        succeed Player6

      _ ->
        "Unknown PlayerIndex: " ++ str |> fail

  )


playerDecoder : Decoder Player
playerDecoder =
  map3 Player (field "totalScore" int) (field "gameScore" int) (field "name" string)


playerNameSetDecoder : Decoder PlayerNameSet
playerNameSetDecoder =
  map6 PlayerNameSet
    (field "name1" string)
    (field "name2" string)
    (field "name3" string)
    (field "name4" string)
    (field "name5" string)
    (field "name6" string)


playerSetDecoder : Decoder PlayerSet
playerSetDecoder =
  map6 PlayerSet
    (field "player1" playerDecoder)
    (field "player2" playerDecoder)
    (field "player3" playerDecoder)
    (field "player4" playerDecoder)
    (field "player5" playerDecoder)
    (field "player6" playerDecoder)


gameStateDecoder : Decoder GameState
gameStateDecoder = map5 GameState
  (field "playerSet" playerSetDecoder)
  (field "firstBidder" playerIndexDecoder)
  (field "myIndex" playerIndexDecoder)
  (field "myCards" cardsDecoder)
  (field "gameId" string)


iBiddingDataDecoder : Decoder IBiddingData
iBiddingDataDecoder = map2 IBiddingData
  (field "highestBidder" playerIndexDecoder)
  (field "highestBid" int)


fBiddingDataDecoder : Decoder FBiddingData
fBiddingDataDecoder = map2 FBiddingData
  (field "biddingWinner" playerIndexDecoder)
  (field "winningBid" int)


biddingDataDecoder : Decoder BiddingData
biddingDataDecoder = oneOf
  [ map IntermediateBiddingData iBiddingDataDecoder
  , map FinalBiddingData fBiddingDataDecoder
  ]


selectionDataDecoder : Decoder SelectionData
selectionDataDecoder = map3 SelectionData
  (field "selectedTrump" suitDecoder)
  (nullable cardDecoder |> field "helper1")
  (nullable cardDecoder |> field "helper2")


playedCardDecoder : Decoder PlayedCard
playedCardDecoder = map2 PlayedCard
  (field "turn" playerIndexDecoder)
  (field "playedCard" cardDecoder)


nextRoundDataDecoder : Decoder NextRoundData
nextRoundDataDecoder = map2 NextRoundData
  (field "firstPlayer" playerIndexDecoder)
  (field "playerSet" playerSetDecoder)


roundDataDecoder : Decoder RoundData
roundDataDecoder = oneOf
  [ map PlayedCardData playedCardDecoder
  , map RoundFinishData nextRoundDataDecoder
  , map GameFinishData gameStateDecoder
  ]