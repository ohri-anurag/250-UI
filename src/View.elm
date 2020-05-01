module View exposing (..)


import Html exposing (Html, button, div, img, input, text)
import Html.Attributes exposing (height, src, style, width)
import Html.Events exposing (on, onClick, onInput)


import Model exposing (..)


viewCard : Card -> Html Msg
viewCard card =
  let
    path = "img/" ++ showCardValue card.value ++ " of " ++ showSuit card.suit ++ ".png"
  in
  img
    [ src path
    , height 200
    , width 131
    ] []


view : Model -> Html Msg
view model =
  case model of
    BeginGamePage str ->
      div []
        [ div [] [text "Welcome to the card game 250!!"]
        , div [] [text "Enter a name for the group:"]
        , input [onInput UpdateGameName] [text str]
        , button [onClick SendGameName] [text "Begin Game"]
        ]

    WaitingForPlayers ->
      div [] [text "Waiting For Players"]

    GameState gState ->
      let
        me = getPlayer gState.players gState.myIndex
        otherPlayers = getPlayers gState.players |> List.filter (\player -> player.index /= gState.myIndex)

        showPlayer player =
          div
            [ style "border" "2px solid black"
            , style "margin" "10px"
            ]
            [ player.info.playerName |> text ]
      in
      div []
        [ List.map showPlayer otherPlayers
          |> div
              [ style "border" "2px solid black"
              , style "display" "flex"
              ]
        , List.map viewCard me.info.cards
          |> div
              [ style "border" "2px solid black" ]
        ]