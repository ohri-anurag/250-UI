module SharedData exposing (..)


import Json.Decode exposing (..)
import String exposing (fromInt)


import Model exposing (..)


type alias InitGameState =
  { index : PlayerIndex
  , cards : List Card
  }


suitDecoder : Decoder Suit
suitDecoder =
  field "suit" string
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
  field "value" string
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
cardDecoder = map2 Card valueDecoder suitDecoder


cardsDecoder : Decoder (List Card)
cardsDecoder =
  list cardDecoder
  |> field "cards"


indexDecoder : Decoder PlayerIndex
indexDecoder =
  field "index" int
  |> andThen (\i ->
    case i of
      1 ->
        succeed Player1

      2 ->
        succeed Player2

      3 ->
        succeed Player3

      4 ->
        succeed Player4

      5 ->
        succeed Player5

      6 ->
        succeed Player6

      _ ->
        "Unknown PlayerIndex: " ++ fromInt i |> fail

  )


decodeInitGameState : Decoder InitGameState
decodeInitGameState = map2 InitGameState indexDecoder cardsDecoder