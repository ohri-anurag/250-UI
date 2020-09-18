module Model.Analytics exposing (..)


type HttpData
  = TotalDataReceived (List PlayerScoreData)


type AnalyticsMode
  = TotalMode (List PlayerScoreData)


type alias PlayerScoreData =
  { score : Int
  , name : String
  , bids : Int
  }