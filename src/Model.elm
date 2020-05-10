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


type PlayerStatus
 = BiddingTeam
 | AntiTeam
 | Undecided


type alias PlayerStatusSet =
  { status1 : PlayerStatus
  , status2 : PlayerStatus
  , status3 : PlayerStatus
  , status4 : PlayerStatus
  , status5 : PlayerStatus
  , status6 : PlayerStatus
  }


type PlayerIndex
  = Player1
  | Player2
  | Player3
  | Player4
  | Player5
  | Player6


type alias Player =
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


type alias BiddingRoundData =
  { playerSet : PlayerSet
  , highestBid : Int
  , highestBidder : PlayerIndex
  , bidders : (List PlayerIndex)
  , myIndex : PlayerIndex
  , amIBidding : Bool
  , myCards : List Card
  , firstBidder : PlayerIndex
  }


type alias TrumpSelectionData =
  { selectionData : SelectionData
  , bid : Int
  , bidder : PlayerIndex
  , playerSet : PlayerSet
  , myIndex : PlayerIndex
  , myCards : List Card
  , firstBidder : PlayerIndex
  }


type TurnStatus
  = FirstAndNotMyTurn PlayerIndex
  | NotFirstAndNotMyTurn PlayerIndex Card
  | FirstAndMyTurn
  | FirstAndMyTurnOver
  | NotFirstAndMyTurn Card
  | NotFirstAndMyTurnOver Card
  | RoundFinished
  | GameFinished


type alias PlayRoundData =
  { trumpSelectionData : TrumpSelectionData
  , firstPlayer : PlayerIndex
  , roundIndex : Round
  , hand : Hand
  , playersStatus : PlayerStatusSet
  , helpersRevealed : Int
  , bidder : PlayerIndex
  , turnStatus : TurnStatus
  }


type Model
  = BeginGamePage String String
  | WaitingForPlayers (List String) String
  | BiddingRound String BiddingRoundData
  | TrumpSelection String TrumpSelectionData
  | WaitingForTrump String BiddingRoundData
  | PlayRound String PlayRoundData



type alias SelectionData =
  { trump : Suit
  , helpers : List Card
  }


type SentMessage
  = IntroData
      String    -- Player name
      String    -- Game name
  | IncreaseBid
      String        -- Game name
      PlayerIndex   -- Bidding Player
      Int           -- Bid Amount
  | SendQuit
      String        -- Game Name
      PlayerIndex   -- My Index
  | SentSelectionData
      String        -- Game Name
      SelectionData -- Selection Data
  | PlayedCard
      String        -- Game Name
      Card          -- The card that was played by me


type ReceivedMessage
  = PlayerJoined
      String            -- Newly Joined Player
  | ExistingPlayers
      (List String)     -- Already existing players in the game
  | GameData
      PlayerSet     -- Set of players
      PlayerIndex   -- The first bidder in this game
      PlayerIndex   -- Your player index
      (List Card)   -- Your cards
  | MaximumBid
      PlayerIndex   -- The player who made the current maximum bid
      Int           -- Bid Amount
  | HasQuitBidding
      PlayerIndex   -- The player who quit bidding
  | ReceivedSelectionData
      SelectionData
  | PlayCard
      Card          -- The card that was played in the turn
  | RoundData
      PlayerIndex   -- Current round's winner, and next round's first player
      Int           -- Score made in current round
  | GameFinishedData
      (List PlayerIndex)  -- Winning Team
      Int                 -- Winning Team score
  | NewGame
      (List Card)         -- Cards for a new game


type Msg
  = UpdatePlayerName String
  | UpdateGameName String
  | SendGameName
  | BidPlus5
  | BidPlus10
  | QuitBidding
  | SelectTrump Suit
  | SelectHelper Card
  | SendTrump
  | SendCard Card
  | NoOp
  | SentMessageType SentMessage
  | ReceivedMessageType ReceivedMessage


allSuits : List Suit
allSuits = [Club, Heart, Diamond, Spade]


allCardValues : List CardValue
allCardValues = [Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]


allCards : List Card
allCards = List.concatMap (\cardValue -> List.map (Card cardValue) allSuits) allCardValues


allPlayerIndices : List PlayerIndex
allPlayerIndices = [Player1, Player2, Player3, Player4, Player5, Player6]


-- otherPlayers : GameState -> List Player
-- otherPlayers gameState =
--   let
--     playerSet = gameState.playerSet
--     allPlayers = getPlayers playerSet
--     me = getPlayer playerSet gameState.myIndex
--   in
--   List.filter (\p -> p /= me) allPlayers

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


nextTurn : PlayerIndex -> PlayerIndex
nextTurn playerIndex =
  case playerIndex of
    Player1 ->
      Player2

    Player2 ->
      Player3

    Player3 ->
      Player4

    Player4 ->
      Player5

    Player5 ->
      Player6

    Player6 ->
      Player1


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


initPlayerStatusSet : PlayerStatusSet
initPlayerStatusSet =
  { status1 = Undecided
  , status2 = Undecided
  , status3 = Undecided
  , status4 = Undecided
  , status5 = Undecided
  , status6 = Undecided
  }



initPlayerSet : PlayerSet
initPlayerSet =
  { player1 = newPlayer Player1
  , player2 = newPlayer Player2
  , player3 = newPlayer Player3
  , player4 = newPlayer Player4
  , player5 = newPlayer Player5
  , player6 = newPlayer Player6
  }


initModel : () -> (Model, Cmd Msg)
initModel _ =
  ( BeginGamePage "" ""
  -- ( BiddingRound "250aadmi"
  --   { playerSet = initPlayerSet
  --   , highestBid = 150
  --   , highestBidder = Player1
  --   , bidders = allPlayerIndices
  --   , amIBidding = True
  --   , myIndex = Player1
  --   , myCards = initCards
  --   , firstBidder = Player1
  --   }
  -- ( TrumpSelection
  --   { selectionData =
  --     { trump = Spade
  --     , helper1 = Nothing
  --     , helper2 = Nothing
  --     }
  --   , bid = 180
  --   , bidder = Player1
  --   , playerSet = initPlayerSet
  --   , myIndex = Player1
  --   , gameName = "250aadmi"
  --   , myCards = initCards
  --   , firstBidder = Player1
  --   }
  -- ( PlayRound "250aadmi"
  --   { trumpSelectionData = 
  --     { selectionData =
  --       { trump = Spade
  --       , helpers = [Card Ace Spade, Card Ace Heart]
  --       }
  --     , bid = 180
  --     , bidder = Player1
  --     , playerSet = initPlayerSet
  --     , myIndex = Player1
  --     , myCards = initCards
  --     , firstBidder = Player1
  --     }
  --   , firstPlayer = Player1
  --   , roundIndex = Round1
  --   , hand = emptyHand
  --   , playersStatus = initPlayerStatusSet
  --   , helpersRevealed = 0
  --   , bidder = Player1
  --   , turn = Just Player1
  --   , haveIPlayed = False
  --   , turnStatus = FirstAndMyTurn
  --   }
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


updatePlayer : PlayerIndex -> (Player -> Player) -> PlayerSet -> PlayerSet
updatePlayer playerIndex update players =
  case playerIndex of
    Player1 ->
      { players | player1 = update players.player1 }

    Player2 ->
      { players | player2 = update players.player2 }

    Player3 ->
      { players | player3 = update players.player3 }

    Player4 ->
      { players | player4 = update players.player4 }

    Player5 ->
      { players | player5 = update players.player5 }

    Player6 ->
      { players | player6 = update players.player6 }


getPlayers : PlayerSet -> List Player
getPlayers players =
  [ players.player1
  , players.player2
  , players.player3
  , players.player4
  , players.player5
  , players.player6
  ]


getPlayerStatuses : PlayerStatusSet -> List (PlayerIndex, PlayerStatus)
getPlayerStatuses playerStatusSet = List.map2 Tuple.pair allPlayerIndices
  [ playerStatusSet.status1
  , playerStatusSet.status2
  , playerStatusSet.status3
  , playerStatusSet.status4
  , playerStatusSet.status5
  , playerStatusSet.status6
  ]


setPlayerStatus : PlayerIndex -> PlayerStatus -> PlayerStatusSet -> PlayerStatusSet
setPlayerStatus playerIndex playerStatus playerStatusSet =
  case playerIndex of
    Player1 ->
      { playerStatusSet
      | status1 = playerStatus
      }

    Player2 ->
      { playerStatusSet
      | status2 = playerStatus
      }

    Player3 ->
      { playerStatusSet
      | status3 = playerStatus
      }

    Player4 ->
      { playerStatusSet
      | status4 = playerStatus
      }

    Player5 ->
      { playerStatusSet
      | status5 = playerStatus
      }

    Player6 ->
      { playerStatusSet
      | status6 = playerStatus
      }


isPlayerHelper : Card -> SelectionData -> Bool
isPlayerHelper card selectionData =
  List.member card selectionData.helpers


amIHelper : List Card -> SelectionData -> Bool
amIHelper myCards selectionData =
  List.any (\h -> List.member h myCards) selectionData.helpers


amITheOnlyHelper : List Card -> SelectionData -> Bool
amITheOnlyHelper myCards selectionData =
  case selectionData.helpers of
    -- Bidder asked for only one helper and that is me
    [card] ->
      List.member card myCards

    -- I have both helpers
    [card1, card2] ->
      List.member card1 myCards && List.member card2 myCards

    _ ->
      False


maxHelpers : SelectionData -> Int
maxHelpers selectionData =
  List.length selectionData.helpers


lookup : a -> List (a, b) -> Maybe b
lookup elem list =
  case list of
    [] ->
      Nothing

    (x :: xs) ->
      if Tuple.first x == elem
        then Tuple.second x |> Just
        else lookup elem xs

