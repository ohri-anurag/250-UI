module Model exposing(..)


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


type alias SelectionData =
  { selectedTrump : Suit
  , helper1 : Maybe Card
  , helper2 : Maybe Card
  }


type alias PlayedCard =
  { turn : PlayerIndex
  , playedCard : Card
  }


type RoundData
  = PlayedCardData PlayedCard
  | RoundFinishData PlayerSet


type Round
 = Round1
 | Round2
 | Round3
 | Round4
 | Round5
 | Round6
 | Round7
 | Round8


type alias Hand =
  { card1 : Maybe Card
  , card2 : Maybe Card
  , card3 : Maybe Card
  , card4 : Maybe Card
  , card5 : Maybe Card
  , card6 : Maybe Card
  }


type alias PlayState =
  { gameState : GameState
  , biddingData : FBiddingData
  , selectionData : SelectionData
  , turn : PlayerIndex
  , hand : Hand
  }


type Model
  = BeginGamePage String String
  | WaitingForPlayers
  | BiddingRound GameState IBiddingData Bool
  | TrumpSelection SelectionData FBiddingData GameState
  | WaitingForTrump FBiddingData GameState
  | PlayRound Round PlayState Bool


type Msg
  = UpdatePlayerName String
  | UpdateGameName String
  | SendGameName
  | BeginGame GameState
  | BidPlus5
  | BidPlus10
  | QuitBidding
  | NewHighestBid PlayerIndex Int
  | FinalBid FBiddingData GameState
  | SelectTrump Suit
  | SelectHelper Card
  | SendTrump
  | StartGameplay PlayState
  | SendCard Card
  | PlayCard Card PlayerIndex
  | NextRound PlayerSet
  | NoOp


allSuits : List Suit
allSuits = [Club, Heart, Diamond, Spade]


allCardValues : List CardValue
allCardValues = [Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]


allCards : List Card
allCards = List.concatMap (\cardValue -> List.map (Card cardValue) allSuits) allCardValues


allPlayerIndices : List PlayerIndex
allPlayerIndices = [Player1, Player2, Player3, Player4, Player5, Player6]


otherPlayers : GameState -> List Player
otherPlayers gameState =
  let
    playerSet = gameState.playerSet
    allPlayers = getPlayers playerSet
    me = getPlayer playerSet gameState.myIndex
  in
  List.filter (\p -> p /= me) allPlayers

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


showRound : Bool -> Round -> String
showRound isJson round =
  case round of
    Round1 ->
      "Round" ++ if isJson then "1" else " 1"

    Round2 ->
      "Round" ++ if isJson then "2" else " 2"

    Round3 ->
      "Round" ++ if isJson then "3" else " 3"

    Round4 ->
      "Round" ++ if isJson then "4" else " 4"

    Round5 ->
      "Round" ++ if isJson then "5" else " 5"

    Round6 ->
      "Round" ++ if isJson then "6" else " 6"

    Round7 ->
      "Round" ++ if isJson then "7" else " 7"

    Round8 ->
      "Round" ++ if isJson then "8" else " 8"


nextRound : Round -> Round
nextRound round =
  case round of
    Round1 ->
      Round2

    Round2 ->
      Round3

    Round3 ->
      Round4

    Round4 ->
      Round5

    Round5 ->
      Round6

    Round6 ->
      Round7

    Round7 ->
      Round8

    Round8 ->
      Round1


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
  , Card Three Spade
  , Card Three Heart
  , Card Three Diamond
  , Card Three Club
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


initSelectionData : SelectionData
initSelectionData =
  { selectedTrump = Spade
  , helper1 = Nothing
  , helper2 = Nothing
  --, helper1 = Just (Card Ace Spade)
  --, helper2 = Just (Card Ace Heart)
  }


emptyHand : Hand
emptyHand =
  { card1 = Nothing
  , card2 = Nothing
  , card3 = Nothing
  , card4 = Nothing
  , card5 = Nothing
  , card6 = Nothing
  }


getCardFromHand : PlayerIndex -> Hand -> Maybe Card
getCardFromHand playerIndex hand =
  case playerIndex of
    Player1 ->
      hand.card1

    Player2 ->
      hand.card2

    Player3 ->
      hand.card3

    Player4 ->
      hand.card4

    Player5 ->
      hand.card5

    Player6 ->
      hand.card6


setCardInHand : PlayerIndex -> Card -> Hand -> Hand
setCardInHand playerIndex card hand =
  case playerIndex of
    Player1 ->
      { hand
      | card1 = Just card
      }

    Player2 ->
      { hand
      | card2 = Just card
      }

    Player3 ->
      { hand
      | card3 = Just card
      }

    Player4 ->
      { hand
      | card4 = Just card
      }

    Player5 ->
      { hand
      | card5 = Just card
      }

    Player6 ->
      { hand
      | card6 = Just card
      }


initPlayState : PlayState
initPlayState = 
  { gameState = initGameState
  , biddingData =
    { biddingWinner = Player1
    , winningBid = 180
    }
  , selectionData = initSelectionData
  , turn = Player1
  , hand = emptyHand
  }


initModel : () -> (Model, Cmd Msg)
initModel _ =
  --( BeginGamePage "" ""
  --( BiddingRound initGameState (initBiddingData Player1)
  ( TrumpSelection initSelectionData { biddingWinner = Player1, winningBid = 180 } initGameState
  --( PlayRound Round1 initPlayState True
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


--intToPlayerIndex : Int -> Maybe PlayerIndex
--intToPlayerIndex num =
--  case num of
--    1 -> Just Player1
--    2 -> Just Player2
--    3 -> Just Player3
--    4 -> Just Player4
--    5 -> Just Player5
--    6 -> Just Player6
--    _ -> Nothing
