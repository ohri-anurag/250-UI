port module Decoders exposing(..)


import Json.Decode exposing (..)


import Model.Card exposing (..)
import Model exposing (..)


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

      "MaximumBid" ->
        map2 MaximumBid
          (field "highestBidder" playerIndexDecoder)
          (field "highestBid" int)

      "HasQuitBidding" ->
        playerIndexDecoder
        |> field "hasQuitBidding"
        |> map HasQuitBidding

      "SelectionData" ->
        map2 SelectionData
          (field "trump" suitDecoder)
          (list cardDecoder |> field "helpers")
        |> andThen (ReceivedSelectionData >> succeed)

      "PlayCard" ->
        field "card" cardDecoder
        |> map PlayCard

      "RoundData" ->
        map2 RoundData
          (field "roundWinner" playerIndexDecoder)
          (field "roundScore" int)

      "GameFinishedData" ->
        map2 GameFinishedData
          (field "winningTeam" (list playerIndexDecoder))
          (field "gameScore" int)

      "NewGame" ->
        field "cards" cardsDecoder
        |> map NewGame

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


playerNameSetDecoder : Decoder PlayerNameSet
playerNameSetDecoder =
  map6 PlayerNameSet
    (field "Player1" string)
    (field "Player2" string)
    (field "Player3" string)
    (field "Player4" string)
    (field "Player5" string)
    (field "Player6" string)
