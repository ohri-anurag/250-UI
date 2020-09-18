module Update.Analytics exposing (..)


import Model exposing (..)
import Model.Analytics exposing (..)


handleHttpData : HttpData -> Model -> (Model, Cmd Msg)
handleHttpData httpData model =
  case httpData of
    TotalDataReceived playerScoreDataList ->
      (TotalMode playerScoreDataList |> AnalyticsPage, Cmd.none)