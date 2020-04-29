module Main exposing(..)


-- External imports
import Browser
import Html exposing (Html, div, text)
import List



-- Local imports
-- import Model exposing (initModel)
-- import Subscriptions exposing (subscriptions)
-- import Update exposing (update)
-- import View exposing (view)

type Suit
  = Club
  | Heart
  | Diamond
  | Spade

type CardValue
  = Ace
  | Two
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

type alias Card =
  { value : CardValue
  , suit : Suit
  }

type alias PlayerData =
  { cards : List Card
  , totalScore : Int
  , gameScore : Int
  }

type PlayerIndex
  = Player1
  | Player2
  | Player3
  | Player4
  | Player5
  | Player6

type alias Player =
  { index : PlayerIndex
  , info : PlayerData
  }

type alias PlayerSet =
  { player1 : Player
  , player2 : Player
  , player3 : Player
  , player4 : Player
  , player5 : Player
  , player6 : Player
  }

type alias Model =
  { players : PlayerSet
  , firstBidder : PlayerIndex
  , biddingWinner : PlayerIndex
  }

forEachPlayer : (PlayerIndex -> a) -> List a
forEachPlayer fn =
  [ fn Player1
  , fn Player2
  , fn Player3
  , fn Player4
  , fn Player5
  , fn Player6
  ]

newPlayer : PlayerIndex -> Player
newPlayer index =
  { index = index
  , info =
    { cards = List.singleton (Card Ace Spade)
    , totalScore = 0
    , gameScore = 0
    }
  }

initModel : Model
initModel =
  let
    playerSet =
      { player1 = newPlayer Player1
      , player2 = newPlayer Player2
      , player3 = newPlayer Player3
      , player4 = newPlayer Player4
      , player5 = newPlayer Player5
      , player6 = newPlayer Player6
      }
  in
  { players = playerSet
  , firstBidder = Player1
  , biddingWinner = Player1
  }


type Msg = Nope

update : Msg -> Model -> Model
update msg model = model

view : Model -> Html Msg
view model = div [] [text "lolol"]


--MAIN
main = Browser.sandbox
  { init = initModel
  , update = update
  , view = view
  }
--   Browser.element
--     { init = initModel
--     , update = update
--     , subscriptions = subscriptions
--     , view = view
--     }