module Model exposing(..)


import Model.Card exposing (..)


type PlayerStatus
 = BiddingTeam
 | AntiTeam
 | Undecided


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
  , card : Maybe Card
  , status : PlayerStatus
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


type alias MyData =
  { myIndex : PlayerIndex
  , myCards : List Card
  }


type alias BiddingData =
  { highestBid : Int
  , highestBidder : PlayerIndex
  , firstBidder : PlayerIndex
  }


type alias CommonData =
  { gameName : String
  , playerSet : PlayerSet           -- Player names and their scores
  , biddingData : BiddingData       -- Highest bid and bidder
  , myData : MyData                 -- My index and cards
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
  { selectionData : SelectionData   -- Selected trump and helpers
  , firstPlayer : PlayerIndex       -- Who plays the first card in a round?
  , roundIndex : Round              -- Which round?
  , helpersRevealed : Int           -- How many helpers have been revealed to me
  , turnStatus : TurnStatus         -- Refer to TurnStatus
  }


type Validation
  = EmptyId
  | EmptyName
  | EmptyGameName
  | DuplicateId
  | DuplicateName


type Model
  = BeginGamePage String String String (Maybe Validation)
  | WaitingForServerValidation String String String
  | WaitingForPlayers (List String) String
  | BiddingRound CommonData (List PlayerIndex)
  | TrumpSelection CommonData SelectionData
  | WaitingForTrump CommonData
  | PlayRound CommonData PlayRoundData
  | ErrorState


type alias SelectionData =
  { trump : Suit
  , helpers : List Card
  }


type SentMessage
  = IntroData
      String    -- Player Id
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
  | BiddingReconnectionData
      PlayerSet
      BiddingData
      MyData
      (List PlayerIndex)  -- Remaining Bidders
  | RoundReconnectionData
      PlayerSet
      BiddingData
      MyData
      SelectionData
      PlayerIndex         -- First player
      PlayerIndex         -- Turn
      Round               -- Which round
  | WebsocketFailed
  | PlayerWithIdAlreadyExists
  | PlayerWithNameAlreadyExists


type Msg
  = UpdatePlayerId String
  | UpdatePlayerName String
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


allPlayerIndices : List PlayerIndex
allPlayerIndices = [Player1, Player2, Player3, Player4, Player5, Player6]


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


newPlayer : PlayerIndex -> Player
newPlayer index =
  { totalScore = 0
  , gameScore = 0
  , name = showPlayerIndex index
  , card = Nothing
  , status = Undecided
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
  ( BeginGamePage "" "" "" Nothing
  -- ( WaitingForPlayers [] ""
  -- ( WaitingForServerValidation "" "" ""
  -- ( BiddingRound
  --   { playerSet = initPlayerSet
  --   , gameName = "250aadmi"
  --   , biddingData =
  --     { highestBid = 150
  --     , highestBidder = Player1
  --     , firstBidder = Player1
  --     }
  --   , myData =
  --     {myIndex = Player1
  --     , myCards = initCards
  --     }
  --   } allPlayerIndices
  -- ( WaitingForTrump
  --   { playerSet = initPlayerSet
  --   , gameName = "250aadmi"
  --   , biddingData =
  --     { highestBid = 150
  --     , highestBidder = Player1
  --     , firstBidder = Player1
  --     }
  --   , myData =
  --     {myIndex = Player1
  --     , myCards = initCards
  --     }
  --   }
  -- ( TrumpSelection
  --   { playerSet = initPlayerSet
  --   , gameName = "250aadmi"
  --   , biddingData =
  --     { highestBid = 150
  --     , highestBidder = Player1
  --     , firstBidder = Player1
  --     }
  --   , myData =
  --     {myIndex = Player1
  --     , myCards = initCards
  --     }
  --   }
  --   { trump = Spade
  --   , helpers = []
  --   }
  -- ( PlayRound
  --   { playerSet = initPlayerSet
  --   , gameName = "250aadmi"
  --   , biddingData =
  --     { highestBid = 150
  --     , highestBidder = Player1
  --     , firstBidder = Player1
  --     }
  --   , myData =
  --     {myIndex = Player1
  --     , myCards = initCards
  --     }
  --   }
  --   { selectionData =
  --     { trump = Spade
  --     , helpers = [Card Ace Spade, Card Ace Heart]
  --     }
  --   , firstPlayer = Player1
  --   , roundIndex = Round3
  --   , helpersRevealed = 0
  --   , turnStatus = FirstAndMyTurn
  --   }
  -- ( ErrorState
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


updateCardInSet : PlayerIndex -> Card -> PlayerSet -> PlayerSet
updateCardInSet playerIndex card = updatePlayer playerIndex (\player ->
    { player | card = Just card }
  )


updatePlayerStatus : PlayerIndex -> PlayerStatus -> PlayerSet -> PlayerSet
updatePlayerStatus playerIndex status = updatePlayer playerIndex (\player ->
    { player | status = status }
  )

getPlayers : (Player -> a) -> PlayerSet -> List (PlayerIndex, a)
getPlayers f players =
  [ (Player1, f players.player1)
  , (Player2, f players.player2)
  , (Player3, f players.player3)
  , (Player4, f players.player4)
  , (Player5, f players.player5)
  , (Player6, f players.player6)
  ]


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

