module Model exposing(..)


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
  { totalScore : Int
  , gameScore : Int
  , playerName : String
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


type alias GState =
  { players : PlayerSet
  , firstBidder : PlayerIndex
  , biddingWinner : PlayerIndex
  , myIndex : PlayerIndex
  , myCards : List Card
  }


type Model
  = BeginGamePage String
  | WaitingForPlayers
  | GameState GState


type Msg
  = UpdateGameName String
  | SendGameName
  | BeginGame PlayerIndex (List Card)
  | NoOp


showSuit : Suit -> String
showSuit suit =
  case suit of
    Club ->
      "Clubs"

    Heart ->
      "Hearts"

    Diamond ->
      "Diamonds"

    Spade ->
      "Spades"


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


--initCards : List Card
--initCards =
--  [ Card Ace Spade
--  , Card Ace Heart
--  , Card Ace Diamond
--  , Card Ace Club
--  , Card Two Spade
--  , Card Two Heart
--  , Card Two Diamond
--  , Card Two Club
--  ]


initGameState : PlayerIndex -> List Card -> GState
initGameState index cards =
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
  , myIndex = index
  , myCards = cards
  }


initModel : () -> (Model, Cmd Msg)
initModel _ =
  ( BeginGamePage ""
  , Cmd.none
  )


showPlayerIndex : PlayerIndex -> String
showPlayerIndex playerIndex =
  case playerIndex of
    Player1 ->
      "Player 1"

    Player2 ->
      "Player 2"

    Player3 ->
      "Player 3"

    Player4 ->
      "Player 4"

    Player5 ->
      "Player 5"

    Player6 ->
      "Player 6"


-- Helpers
newPlayer : PlayerIndex -> Player
newPlayer index =
  { index = index
  , info =
    { totalScore = 0
    , gameScore = 0
    , playerName = showPlayerIndex index
    }
  }


--getPlayer : PlayerSet -> PlayerIndex -> Player
--getPlayer players index =
--  case index of
--    Player1 ->
--      players.player1

--    Player2 ->
--      players.player2

--    Player3 ->
--      players.player3

--    Player4 ->
--      players.player4

--    Player5 ->
--      players.player5

--    Player6 ->
--      players.player6


getPlayers : PlayerSet -> List Player
getPlayers players =
  [ players.player1
  , players.player2
  , players.player3
  , players.player4
  , players.player5
  , players.player6
  ]



intToPlayerIndex : Int -> Maybe PlayerIndex
intToPlayerIndex num =
  case num of
    1 -> Just Player1
    2 -> Just Player2
    3 -> Just Player3
    4 -> Just Player4
    5 -> Just Player5
    6 -> Just Player6
    _ -> Nothing