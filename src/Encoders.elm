port module Encoders exposing (..)


import Json.Encode as E


import Model exposing (..)


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

    IncreaseBid gameName bidder bid ->
      increaseBidEncoder gameName bidder bid

    SendQuit gameName myIndex ->
      quitBiddingEncoder gameName myIndex

    SentSelectionData gameName selectionData ->
      selectionDataEncoder gameName selectionData.trump selectionData.helpers

    PlayedCard gameName card ->
      playedCardEncoder gameName card


introDataEncoder : String -> String -> E.Value
introDataEncoder playerName gameName = E.object
  [ ("gameName", E.string gameName)
  , ("value", E.object
      [ ("tag", E.string "IntroData")
      , ("playerName", E.string playerName)
      ]
    )
  ]


increaseBidEncoder : String -> PlayerIndex -> Int -> E.Value
increaseBidEncoder gameName bidder bid = E.object
  [ ("gameName", E.string gameName)
  , ("value", E.object
      [ ("tag", E.string "IncreaseBid")
      , ("bidder", showPlayerIndex bidder |> E.string)
      , ("bid", E.int bid)
      ]
    )
  ]


quitBiddingEncoder : String -> PlayerIndex -> E.Value
quitBiddingEncoder gameName myIndex = E.object
  [ ("gameName", E.string gameName)
  , ("value", E.object
      [ ("tag", E.string "QuitBidding")
      , ("quitter", showPlayerIndex myIndex |> E.string)
      ]
    )
  ]


selectionDataEncoder : String -> Suit -> List Card -> E.Value
selectionDataEncoder gameName suit helpers = E.object
  [ ("gameName", E.string gameName)
  , ("value", E.object
      [ ("tag", E.string "SelectionData")
      , ("trump", showSuit False suit |> E.string)
      , ("helpers", E.list cardEncoder helpers)
      ]
    )
  ]


playedCardEncoder : String -> Card -> E.Value
playedCardEncoder gameName card = E.object
  [ ("gameName", E.string gameName)
  , ("value", E.object
      [ ("tag", E.string "PlayedCard")
      , ("playedCard", cardEncoder card)
      ]
    )
  ]


cardEncoder : Card -> E.Value
cardEncoder card = E.object
  [ ("value", showCardValue card.value |> E.string)
  , ("suit", showSuit False card.suit |> E.string)
  ]
