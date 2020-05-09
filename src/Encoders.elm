port module Encoders exposing (..)


import Json.Encode as E


import Model exposing (..)
-- import SharedData exposing (SentData(..))


port messageSender : String -> Cmd msg


sendMessage : SentMessage -> Cmd msg
sendMessage sentData =
  sentDataEncoder sentData
  |> E.encode 0
  |> messageSender


sentDataEncoder : SentMessage -> E.Value
sentDataEncoder sentData =
  case sentData of
    IntroData playerName gameName ->
      introDataEncoder playerName gameName


introDataEncoder : String -> String -> E.Value
introDataEncoder playerName gameName = E.object
  [ ("gameName", E.string gameName)
  , ("value", E.object
      [ ("tag", E.string "IntroData")
      , ("playerName", E.string playerName)
      ]
    )
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
      E.null


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


encodePlayedCard : String -> Card -> E.Value
encodePlayedCard gameName card = E.object
  [ ("gameName", E.string gameName)
  , ( "card", encodeCard card)
  ]