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


--type alias PlayerData =
--  { totalScore : Int
--  , gameScore : Int
--  , playerName : String
--  }


type PlayerIndex
  = Player1
  | Player2
  | Player3
  | Player4
  | Player5
  | Player6


type alias Player =
  --{ index : PlayerIndex
  --, info : PlayerData
  --}
  { totalScore : Int
  , gameScore : Int
  , name : String
  }


type alias PlayerSet =
  { player1 : Player
  , player2 : Player
  , player3 : Player
  , player4 : Player
  , player5 : Player
  , player6 : Player
  }


type alias GameState =
  { playerSet : PlayerSet
  , firstBidder : PlayerIndex           -- This person will play the first turn, and have the first chance at bidding.
  , myIndex : PlayerIndex
  , myCards : List Card
  , gameName : String
  }


type BiddingData
  = IntermediateBiddingData IBiddingData
  | FinalBiddingData FBiddingData


type alias IBiddingData =
  { highestBidder : PlayerIndex
  , highestBid : Int
  }


type alias FBiddingData =
  { biddingWinner : PlayerIndex
  , winningBid : Int
  }


type Model
  = BeginGamePage String String
  | WaitingForPlayers
  | BiddingRound GameState IBiddingData Bool
  | Round1


type Msg
  = UpdatePlayerName String
  | UpdateGameName String
  | SendGameName
  | BeginGame GameState
  | BidPlus5
  | BidPlus10
  | QuitBidding
  | NewHighestBid PlayerIndex Int
  | FinalBid PlayerIndex Int
  | NoOp


--type alias InitGameState =
--  { index : PlayerIndex
--  , cards : List Card
--  , playerSet: PlayerSet
--  }

otherPlayers : GameState -> List Player
otherPlayers gameState =
  let
    playerSet = gameState.playerSet
    allPlayers = getPlayers playerSet
    me = getPlayer playerSet gameState.myIndex
  in
  List.filter (\p -> p /= me) allPlayers

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


initBiddingData : PlayerIndex -> IBiddingData
initBiddingData playerIndex =
  { highestBid = 150
  , highestBidder = playerIndex
  }

initCards : List Card
initCards =
  [ Card Ace Spade
  , Card Ace Heart
  , Card Ace Diamond
  , Card Ace Club
  , Card Two Spade
  , Card Two Heart
  , Card Two Diamond
  , Card Two Club
  ]

newPlayer : PlayerIndex -> Player
newPlayer index =
  { totalScore = 0
  , gameScore = 0
  , name = showPlayerIndex index
  }

initGameState : GameState
initGameState =
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
  { playerSet = playerSet
  , firstBidder = Player1
  , myIndex = Player1
  , myCards = initCards
  , gameName = "250aadmi"
  }


initModel : () -> (Model, Cmd Msg)
initModel _ =
  ( BeginGamePage "" ""
  --( BiddingRound initGameState (initBiddingData Player1)
  , Cmd.none
  )


showPlayerIndex : PlayerIndex -> String
showPlayerIndex playerIndex =
  case playerIndex of
    Player1 ->
      "Player1"

    Player2 ->
      "Player2"

    Player3 ->
      "Player3"

    Player4 ->
      "Player4"

    Player5 ->
      "Player5"

    Player6 ->
      "Player6"


-- Helpers
getPlayer : PlayerSet -> PlayerIndex -> Player
getPlayer players index =
  case index of
    Player1 ->
      players.player1

    Player2 ->
      players.player2

    Player3 ->
      players.player3

    Player4 ->
      players.player4

    Player5 ->
      players.player5

    Player6 ->
      players.player6


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