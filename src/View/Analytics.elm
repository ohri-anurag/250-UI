module View.Analytics exposing (..)


import Html exposing (Html, div, h1, h3, text, table, tr, th, td)
import Html.Attributes exposing (attribute)
import String exposing (fromInt)


import Model exposing (..)
import Model.Analytics exposing (..)


analyticsPageView : AnalyticsMode -> Html Msg
analyticsPageView analyticsMode =
  div [attribute "class" "analyticsContainer"] <|
    [ h1 [attribute "class" "analyticsHeader"] [text "Scorecard"]
    , div
        [attribute "class" "analyticsContent"]
        [analyticsModeView analyticsMode]
    ]


analyticsModeView : AnalyticsMode -> Html Msg
analyticsModeView analyticsMode =
  case analyticsMode of
    TotalMode playerScoreDataList ->
      totalModeView playerScoreDataList


totalModeView : List PlayerScoreData -> Html Msg
totalModeView playerScoreDataList =
  let
    sortedList = List.reverse <| List.sortBy .score playerScoreDataList
  in
  [ h3 [attribute "class" "totalScoreHeader"] [text "Highest Total Score"]
  , tr
      []
      [ th [] [text "S. No."]
      , th [] [text "Player Name"]
      , th [] [text "Total Score"]
      , th [] [text "Games Played"]
      , th [] [text "Total Bids"]
      , th [] [text "Successful Bids"]
      ]
    :: List.indexedMap playerScoreDataView sortedList
    |> table
        [ attribute "class" "totalScoreTable"
        , attribute "cellpadding" "0px"
        , attribute "cellspacing" "0px"
        ]
  ]
  |> div [attribute "class" "totalScoreView"]
  |> List.singleton
  |> div [attribute "class" "totalModeView"]


playerScoreDataView : Int -> PlayerScoreData -> Html Msg
playerScoreDataView index playerScoreData =
  tr
    [attribute "class" <| if modBy 2 index == 0 then "even" else "odd"]
    [ td [] [index + 1 |> fromInt |> text]
    , td [] [text playerScoreData.name]
    , td [] [fromInt playerScoreData.score |> text]
    , td [] [fromInt playerScoreData.games |> text]
    , td [] [fromInt playerScoreData.bids |> text]
    , td [] [fromInt playerScoreData.successful_bids |> text]
    ]
