module SharedData exposing (..)


import Json.Decode exposing (..)
import Json.Encode as E


import Model exposing (..)


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


encodeIntroData : String -> String -> E.Value
encodeIntroData playerName gameName = E.object
  [ ("playerName", E.string playerName)
  , ("gameName", E.string gameName)
  ]


encodeBiddingData : String -> PlayerIndex -> Int -> E.Value
encodeBiddingData gameName myIndex myBid = E.object
  [ ("gameName", E.string gameName)
  , ("playerIndex", showPlayerIndex myIndex |> E.string)
  , ("bid", E.int myBid)
  ]


encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe encoder maybe =
  case maybe of
    Just x ->
      encoder x

    Nothing ->
      E.string "null"


encodeCard : Card -> E.Value
encodeCard card = E.object
  [ ("value", showCardValue card.value |> E.string)
  , ("suit", showSuit False card.suit |> E.string)
  ]


encodeSelectionData : String -> SelectionData -> E.Value
encodeSelectionData gameName selectionData = E.object
  [ ("gameName", E.string gameName)
  , ( "value"
    , E.object
      [ ("selectedTrump", showSuit False selectionData.selectedTrump |> E.string)
      , ("helper1", encodeMaybe encodeCard selectionData.helper1)
      , ("helper2", encodeMaybe encodeCard selectionData.helper2)
      ]
    )
  ]