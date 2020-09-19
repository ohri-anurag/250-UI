module Model.Analytics exposing (..)


type HttpData
  = TotalDataReceived (List PlayerScoreData)


type AnalyticsMode
  = TotalMode (List PlayerScoreData)


type alias PlayerScoreData =
  { name : String
  , score : Int
  , games : Int
  , bids : Int
  , successful_bids : Int
  }