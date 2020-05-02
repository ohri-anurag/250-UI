module View exposing (..)


import Html exposing (Html, button, div, img, input, span, text)
import Html.Attributes exposing (attribute, height, src, style, width)
import Html.Events exposing (on, onClick, onInput)
import String exposing (fromInt)


import Model exposing (..)


view : Model -> Html Msg
view model =
  case model of
    BeginGamePage playerName gameName ->
      div []
        [ div [] [text "Welcome to the card game 250!!"]
        , div [] [text "Enter your display name:"]
        , input [onInput UpdatePlayerName] [text playerName]
        , div [] [text "Enter a name for the group:"]
        , input [onInput UpdateGameName] [text gameName]
        , button [onClick SendGameName] [text "Begin Game"]
        ]

    WaitingForPlayers ->
      div [] [text "Waiting For Players"]

    BiddingRound gameState biddingData isBidding ->
      let
        me = getPlayer gameState.playerSet gameState.myIndex
        highestBidderName =
          if biddingData.highestBidder == gameState.myIndex
            then "You"
            else
              getPlayer gameState.playerSet biddingData.highestBidder
              |> .name
      in
      div []
        [ gameNameView gameState.gameName
        , otherPlayersView gameState
        , biddingZoneView highestBidderName biddingData isBidding
        , div
            [ attribute "class" "myCardsContainer" ]
            [  div
              [ attribute "class" "myName" ]
              [ text "Your cards" ]
            , List.map cardView gameState.myCards
              |> div
                [ attribute "class" "myCards" ]
            , div
                [ attribute "class" "myScores" ]
                [ div [] [ "This game's score: " ++ fromInt me.gameScore |> text ]
                , div [] [ "Total Score: " ++ fromInt me.totalScore |> text ]
                ]
            ]
        ]

    Round1 ->
      div [] [text "Round 1"]


gameNameView : String -> Html Msg
gameNameView name =
  div
    [ attribute "class" "gameName" ]
    [ text name ]


otherPlayersView : GameState -> Html Msg
otherPlayersView gameState =
  otherPlayers gameState
    |> List.indexedMap playerView
    |> div
        [ attribute "class" "players" ]
    |> List.singleton
    |> div
        [ attribute "class" "playersContainer" ]


biddingZoneView : String -> IBiddingData -> Bool -> Html Msg
biddingZoneView highestBidderName biddingData isBidding =
  let
    biddingHtml =
      if isBidding
        then
          [ div
            [attribute "class" "bidButtonContainer"]
            [ button
              [ attribute "class" "bidButton"
              , onClick BidPlus5
              ]
              [text "+5"]
            , button
              [ attribute "class" "bidButton"
              , onClick BidPlus10
              ]
              [text "+10"]
            ]
          , button
            [ attribute "class" "quitBiddingButton"
            , onClick QuitBidding
            ]
            [text "Quit Bidding"]
          ]
        else
          [ div [] [text "You can't bid anymore."]
          ]
  in
  [ span
    [attribute "class" "bidValueLabel"]
    [text "Highest Bid"]
  , span
    [attribute "class" "bidValue"]
    [fromInt biddingData.highestBid |> text]
  , span []
    ["(" ++ highestBidderName ++ ")" |> text]
  ]
  ++
  biddingHtml
  |> div
    [ attribute "class" "biddingZone" ]


playerView : Int -> Player -> Html Msg
playerView i player =
  div
  [ "player p" ++ fromInt i |> attribute "class" ]
  [ player.name
    |> text
    |> List.singleton
    |> span [ attribute "class" "playerName" ]
  , span
    [ attribute "class" "playerScore" ]
    [ fromInt player.totalScore |> text ]
  , span
    [ attribute "class" "playerScoreLabel" ]
    [ text "Total" ]
  , span
    [ attribute "class" "playerScore" ]
    [ fromInt player.gameScore |> text ]
  , span
    [ attribute "class" "playerScoreLabel" ]
    [ text "Current score" ]
  ]


cardView : Card -> Html Msg
cardView card =
  let
    path = "img/" ++ showCardValue card.value ++ " of " ++ showSuit card.suit ++ ".png"
  in
  img
    [ src path
    ] []
