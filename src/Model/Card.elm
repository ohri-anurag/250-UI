module Model.Card exposing (..)


type Suit
  = Club
  | Heart
  | Diamond
  | Spade


type CardValue
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace


type alias Card =
  { value : CardValue
  , suit : Suit
  }


-- Listing Functions
allSuits : List Suit
allSuits = [Club, Heart, Diamond, Spade]


allCardValues : List CardValue
allCardValues = [Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]


allCards : List Card
allCards = List.concatMap (\cardValue -> List.map (Card cardValue) allSuits) allCardValues


-- Show Functions
showSuit : Bool -> Suit -> String
showSuit isPlural suit =
  let
    suitStr =
      case suit of
        Club ->
          "Club"

        Heart ->
          "Heart"

        Diamond ->
          "Diamond"

        Spade ->
          "Spade"

  in
  suitStr ++ if isPlural then "s" else ""


showCardValue : CardValue -> String
showCardValue cardValue =
  case cardValue of
    Ace ->
      "Ace"

    Two ->
      "Two"

    Three ->
      "Three"

    Four ->
      "Four"

    Five ->
      "Five"

    Six ->
      "Six"

    Seven ->
      "Seven"

    Eight ->
      "Eight"

    Nine ->
      "Nine"

    Ten ->
      "Ten"

    Jack ->
      "Jack"

    Queen ->
      "Queen"

    King ->
      "King"


-- Testing functions
initCards : List Card
initCards =
  [ Card Ace Spade
  , Card Ace Heart
  , Card Ace Diamond
  , Card Ace Club
  , Card Three Spade
  , Card Three Heart
  , Card Three Diamond
  , Card Three Club
  ]
