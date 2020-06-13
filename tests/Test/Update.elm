module Test.Update exposing (..)

import Expect exposing (..)
import Fuzz exposing (..)
import Random exposing (maxInt, uniform)
import Shrink exposing (noShrink)
import Test exposing (..)
import Tuple exposing (first, second)


import Model.Card exposing (..)
import Model exposing (..)
import Update exposing (..)


nonEmptyString : Fuzzer String
nonEmptyString = map (
  \str ->
    case str of
      "" ->
        "a"

      _ ->
        str
  ) string


nonEmptyList : Fuzzer a -> Fuzzer (List a)
nonEmptyList itemFuzzer =
  map2 (\items item ->
    case items of
      [] ->
        [item]

      _ ->
        items

  ) (list itemFuzzer) itemFuzzer


limitedLengthList : Int -> Fuzzer (List a) -> Fuzzer (List a)
limitedLengthList length itemsFuzzer =
  map (\items ->
    if List.length items > length
      then List.take length items
      else items
  ) itemsFuzzer

playerIndexFuzzer : Fuzzer PlayerIndex
playerIndexFuzzer = custom (uniform Player1 [Player2, Player3, Player4, Player5, Player6]) noShrink


suitFuzzer : Fuzzer Suit
suitFuzzer = custom (uniform Spade [Diamond, Club, Heart]) noShrink


cardValueFuzzer : Fuzzer CardValue
cardValueFuzzer = custom (uniform Three [Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace]) noShrink


cardFuzzer : Fuzzer Card
cardFuzzer = map2 Card cardValueFuzzer suitFuzzer


commonDataFuzzer : Fuzzer Int -> Fuzzer CommonData
commonDataFuzzer bidFuzzer =
  map5
  (\gameName highestBidder firstBidder myIndex bid ->
    { gameName = gameName
    , playerSet = testPlayerSet
    , biddingData =
      { highestBid = bid
      , highestBidder = highestBidder
      , firstBidder = firstBidder
      }
    , myData =
      { myCards = testCards
      , myIndex = myIndex
      }
    }
  ) string playerIndexFuzzer playerIndexFuzzer playerIndexFuzzer bidFuzzer


selectionDataFuzzer : Fuzzer (List Card) -> Fuzzer SelectionData
selectionDataFuzzer helperFuzzer =
  map2 (\suit helpers ->
    { trump = suit
    , helpers = helpers
    }
  ) suitFuzzer helperFuzzer


threeCards : Fuzzer (List Card)
threeCards =
  intRange 0 (List.length allCards - 4)
  |> map (\i ->
    List.take 3 <| List.drop i allCards
  )


roundIndexFuzzer : Fuzzer Round
roundIndexFuzzer = custom (uniform Round1 [Round2, Round3, Round4, Round5, Round6, Round7, Round8]) noShrink


turnStatusFuzzer : Fuzzer TurnStatus
turnStatusFuzzer =
  oneOf
    [ map FirstAndNotMyTurn playerIndexFuzzer
    , map2 NotFirstAndNotMyTurn playerIndexFuzzer cardFuzzer
    , constant FirstAndMyTurn
    , constant FirstAndMyTurnOver
    , map NotFirstAndMyTurn cardFuzzer
    , map NotFirstAndMyTurnOver cardFuzzer
    , constant RoundFinished
    , constant GameFinished
    ]


playRoundDataFuzzer : Fuzzer PlayRoundData
playRoundDataFuzzer =
  map5 (\selectionData firstPlayer roundIndex helpersRevealed turnStatus ->
    { selectionData = selectionData
    , firstPlayer = firstPlayer
    , roundIndex = roundIndex
    , helpersRevealed = helpersRevealed
    , turnStatus = turnStatus
    }
  ) (selectionDataFuzzer <| limitedLengthList 2 <| list cardFuzzer) playerIndexFuzzer roundIndexFuzzer (intRange 0 2) turnStatusFuzzer


gameDataFuzzer : Fuzzer ReceivedMessage
gameDataFuzzer =
  map3 (GameData testPlayerSet) playerIndexFuzzer playerIndexFuzzer (list cardFuzzer)


maximumBidFuzzer : Fuzzer ReceivedMessage
maximumBidFuzzer =
  map2 MaximumBid playerIndexFuzzer <| intRange 150 250


testPlayer : PlayerIndex -> Player
testPlayer playerIndex =
  { totalScore = 0
  , gameScore = 0
  , name = showPlayerIndex playerIndex
  , card = Nothing
  , status = Undecided
  }


testPlayerSet : PlayerSet
testPlayerSet =
  { player1 = testPlayer Player1
  , player2 = testPlayer Player2
  , player3 = testPlayer Player3
  , player4 = testPlayer Player4
  , player5 = testPlayer Player5
  , player6 = testPlayer Player6
  }


testCards : List Card
testCards =
  [ Card Ace Spade
  , Card Ace Diamond
  , Card Ace Heart
  , Card Ace Club
  , Card King Spade
  , Card King Diamond
  , Card King Heart
  , Card King Club
  ]


testCommonData : CommonData
testCommonData =
  { gameName = "g"
  , playerSet = testPlayerSet
  , biddingData =
    { highestBid = 150
    , highestBidder = Player2
    , firstBidder = Player2
    }
  , myData =
    { myCards = []
    , myIndex = Player1
    }
  }


testSelectionData : SelectionData
testSelectionData =
  { trump = Spade
  , helpers = []
  }


testPlayRoundData : PlayRoundData
testPlayRoundData =
  { selectionData = testSelectionData
  , firstPlayer = Player2
  , roundIndex = Round3
  , helpersRevealed = 0
  , turnStatus = FirstAndMyTurn
  }


changeBid : Int -> CommonData
changeBid b =
  { gameName = "g"
  , playerSet = testPlayerSet
  , biddingData =
    { highestBid = b
    , highestBidder = Player2
    , firstBidder = Player2
    }
  , myData =
    { myCards = []
    , myIndex = Player1
    }
  }

testStateAndCommand : String -> Msg -> Model -> (Model, Maybe SentMessage) -> Test
testStateAndCommand testDescription message state (finalState, command) =
  test testDescription <|
    \_ ->
      case updateHelper message state of
        stateAndCommand ->
          all
            [ first >> equal finalState
            , second >> equal command
            ]
            stateAndCommand


-- User Interaction Tests
suiteUpdateHelper : Test
suiteUpdateHelper =
  describe "updateHelper function"
    [ fuzz string "updates player id properly" <|
        \playerId ->
          case updateHelper (UpdatePlayerId playerId) (BeginGamePage "" "" "" Nothing) of
            stateAndCommand ->
              equal stateAndCommand (BeginGamePage playerId "" "" Nothing, Nothing)

    , fuzz string "updates player name properly" <|
        \playerName ->
          case updateHelper (UpdatePlayerName playerName) (BeginGamePage "" "" "" Nothing) of
            stateAndCommand ->
              equal stateAndCommand (BeginGamePage "" playerName "" Nothing, Nothing)

    , fuzz string "updates game name properly" <|
        \gameName ->
          case updateHelper (UpdateGameName gameName) (BeginGamePage "" "" "" Nothing) of
            stateAndCommand ->
              equal stateAndCommand (BeginGamePage "" "" gameName Nothing, Nothing)

    , fuzz3 nonEmptyString nonEmptyString nonEmptyString
        """
        changes state to WaitingForServerValidation when player clicks on Begin game with valid id, name and game
        """ <|
        \playerId playerName gameName ->
          case updateHelper SendGameName (BeginGamePage playerId playerName gameName Nothing) of
            stateAndCommand ->
              equal
                stateAndCommand
                ( WaitingForServerValidation playerId playerName gameName
                , IntroData  playerId playerName gameName |> Just
                )

    , fuzz2 string string
        "doesn't changes state to WaitingForPlayers when player clicks on Begin game with invalid id" <|
        \playerName gameName ->
          case updateHelper SendGameName (BeginGamePage "" playerName gameName Nothing) of
            stateAndCommand ->
              equal
                stateAndCommand
                (BeginGamePage "" playerName gameName <| Just EmptyId, Nothing)

    , fuzz2 nonEmptyString string
        "doesn't changes state to WaitingForPlayers when player clicks on Begin game with invalid name" <|
        \playerId gameName ->
          case updateHelper SendGameName (BeginGamePage playerId "" gameName Nothing) of
            stateAndCommand ->
              equal
                stateAndCommand
                (BeginGamePage playerId "" gameName <| Just EmptyName, Nothing)

    , fuzz2 nonEmptyString nonEmptyString
        "doesn't changes state to WaitingForPlayers when player clicks on Begin game with invalid game" <|
        \playerId playerName ->
          case updateHelper SendGameName (BeginGamePage playerId playerName "" Nothing) of
            stateAndCommand ->
              equal
                stateAndCommand
                (BeginGamePage playerId playerName "" <| Just EmptyGameName, Nothing)

    , fuzz2 (list playerIndexFuzzer) (commonDataFuzzer (intRange 150 240))
        "updates the bid in Bidding Round properly by 5 (new bid less than 250)" <|
        \bidders commonData ->
          case updateHelper BidPlus5 (BiddingRound commonData bidders) of
            stateAndCommand ->
              equal
                stateAndCommand
                ( BiddingRound commonData bidders
                , commonData.biddingData.highestBid + 5
                  |> IncreaseBid commonData.gameName commonData.myData.myIndex
                  |> Just
                )

    , fuzz2 (list playerIndexFuzzer) (commonDataFuzzer (intRange 245 maxInt))
        "updates the bid in Bidding Round properly by 5 (new bid not less than 250)" <|
        \bidders commonData ->
          case updateHelper BidPlus5 (BiddingRound commonData bidders) of
            stateAndCommand ->
              equal
                stateAndCommand
                ( BiddingRound commonData []
                , IncreaseBid commonData.gameName commonData.myData.myIndex 250
                  |> Just
                )

    , fuzz2 (list playerIndexFuzzer) (commonDataFuzzer (intRange 150 235))
        "updates the bid in Bidding Round properly by 10 (new bid less than 250)" <|
        \bidders commonData ->
          case updateHelper BidPlus10 (BiddingRound commonData bidders) of
            stateAndCommand ->
              equal
                stateAndCommand
                ( BiddingRound commonData bidders
                , commonData.biddingData.highestBid + 10
                  |> IncreaseBid commonData.gameName commonData.myData.myIndex
                  |> Just
                )

    , fuzz2 (list playerIndexFuzzer) (commonDataFuzzer (intRange 240 maxInt))
        "updates the bid in Bidding Round properly by 10 (new bid not less than 250)" <|
        \bidders commonData ->
          case updateHelper BidPlus10 (BiddingRound commonData bidders) of
            stateAndCommand ->
              equal
                stateAndCommand
                ( BiddingRound commonData []
                , IncreaseBid commonData.gameName commonData.myData.myIndex 250
                  |> Just
                )

    , fuzz2 (commonDataFuzzer (intRange 150 250)) (list playerIndexFuzzer)
        "removes me from bidders when I click Quit Bidding" <|
        \commonData bidders ->
          case updateHelper QuitBidding (BiddingRound commonData bidders) of
            stateAndCommand ->
              equal
                stateAndCommand
                ( BiddingRound commonData <| List.filter ((/=) commonData.myData.myIndex) bidders
                , SendQuit commonData.gameName commonData.myData.myIndex |> Just
                )

    , fuzz3 suitFuzzer (commonDataFuzzer (intRange 150 250)) (selectionDataFuzzer <| limitedLengthList 2 <| list cardFuzzer)
        "selects trump when I click on it" <|
        \trump commonData selectionData ->
          case updateHelper (SelectTrump trump) (TrumpSelection commonData selectionData) of
            stateAndCommand ->
              equal
                stateAndCommand
                ( TrumpSelection commonData { selectionData | trump = trump }
                , Nothing
                )

    , fuzz3 cardFuzzer (commonDataFuzzer (intRange 150 250)) (selectionDataFuzzer (constant []))
        "selects new helper when it is not selected" <|
        \card commonData selectionData ->
          case updateHelper (SelectHelper card) (TrumpSelection commonData selectionData) of
            stateAndCommand ->
              equal
                stateAndCommand
                ( TrumpSelection commonData { selectionData | helpers = [card] }
                , Nothing
                )

    , fuzz2 (commonDataFuzzer (intRange 150 250)) (selectionDataFuzzer <| limitedLengthList 2 <| nonEmptyList cardFuzzer)
        "deselects helper when it is already selected" <|
        \commonData selectionData ->
          case selectionData.helpers of
            [] ->
              fail "Selection Data was empty"

            (card :: _) ->
              case updateHelper (SelectHelper card) (TrumpSelection commonData selectionData) of
                stateAndCommand ->
                  equal
                    stateAndCommand
                    ( TrumpSelection commonData { selectionData | helpers = List.filter ((/=) card) selectionData.helpers }
                    , Nothing
                    )

    , fuzz3 threeCards (commonDataFuzzer (intRange 150 250)) (selectionDataFuzzer <| constant [])
        "does nothing when both helpers have been selected" <|
          \cards commonData selectionData ->
            case cards of
              (c1 :: (c2 :: [c3])) ->
                case updateHelper (SelectHelper c3) (TrumpSelection commonData { selectionData | helpers = [c1, c2] }) of
                  stateAndCommand ->
                    equal
                      stateAndCommand
                      ( TrumpSelection commonData { selectionData | helpers = [c1, c2] }
                      , Nothing
                      )

              _ ->
                fail "Three cards were not returned"

    , fuzz2 (commonDataFuzzer (intRange 150 250)) (selectionDataFuzzer <| limitedLengthList 2 <| list cardFuzzer)
        "sends selection data to server when proceed button is clicked" <|
          \commonData selectionData ->
            case updateHelper SendTrump (TrumpSelection commonData selectionData) of
              stateAndCommand ->
                equal
                  stateAndCommand
                  ( TrumpSelection commonData selectionData
                  , SentSelectionData commonData.gameName selectionData |> Just
                  )

    , fuzz3 cardFuzzer (commonDataFuzzer (intRange 150 250)) playRoundDataFuzzer
        "send card data to server when a card is clicked, I have the first turn" <|
        \card commonData playRoundData ->
          case updateHelper (SendCard card) (PlayRound commonData { playRoundData | turnStatus = FirstAndMyTurn}) of
            stateAndCommand ->
              equal
                stateAndCommand
                ( PlayRound commonData { playRoundData | turnStatus = FirstAndMyTurnOver }
                , PlayedCard commonData.gameName card |> Just
                )

    , fuzz3 cardFuzzer (commonDataFuzzer (intRange 150 250)) playRoundDataFuzzer
        "send card data to server when a card is clicked, I don't have the first turn" <|
        \card commonData playRoundData ->
          case updateHelper (SendCard card) (PlayRound commonData { playRoundData | turnStatus = NotFirstAndMyTurn card}) of
            stateAndCommand ->
              equal
                stateAndCommand
                ( PlayRound commonData { playRoundData | turnStatus = NotFirstAndMyTurnOver card }
                , PlayedCard commonData.gameName card |> Just
                )

    -- TODO: Test this for all states
    , fuzz3 string string string
        "does not update state when NoOp is received" <|
          \playerId playerName gameName ->
            case updateHelper NoOp (BeginGamePage playerId playerName gameName Nothing) of
              stateAndCommand ->
                equal
                  stateAndCommand
                  (BeginGamePage playerId playerName gameName Nothing, Nothing)

    -- TODO: Test this for all states
    , fuzz3 string string string
        "does not update state when a SentMessage is received" <|
          \playerId playerName gameName ->
            case updateHelper (IntroData playerId playerName gameName |> SentMessageType) (BeginGamePage playerId playerName gameName Nothing) of
              stateAndCommand ->
                equal
                  stateAndCommand
                  (BeginGamePage playerId playerName gameName Nothing, Nothing)
    ]

-- Received Messages Tests
suiteUpdateReceivedData : Test
suiteUpdateReceivedData =
  describe "handleReceivedMessages function"
    [ fuzz3 string (list string) string "updates the waiting for players list when a new player joins" <|
        \playerName existingPlayers gameName ->
          case handleReceivedMessages (PlayerJoined playerName) (WaitingForPlayers existingPlayers gameName) of
            stateAndCommand ->
              equal
                stateAndCommand
                ( WaitingForPlayers (existingPlayers ++ [playerName]) gameName
                , Nothing
                )

    , fuzz3 (list string) string string
        "updates the state to WaitingForPlayers when list of existing players is received" <|
          \existingPlayers gameName playerName ->
            case handleReceivedMessages (ExistingPlayers existingPlayers) (WaitingForServerValidation "" playerName gameName) of
              stateAndCommand ->
                equal
                  stateAndCommand
                  ( WaitingForPlayers (existingPlayers ++ [playerName]) gameName
                  , Nothing
                  )

    , fuzz3 gameDataFuzzer (list string) string
        "updates the state to BiddingRound when GameData is received" <|
          \gameData bidders gameName ->
            case gameData of
              GameData playerSet firstBidder myIndex myCards ->
                case handleReceivedMessages gameData (WaitingForPlayers bidders gameName) of
                  stateAndCommand ->
                    equal
                      stateAndCommand
                      ( BiddingRound
                          { gameName = gameName
                          , playerSet = playerSet
                          , biddingData =
                            { highestBid = 150
                            , highestBidder = firstBidder
                            , firstBidder = firstBidder
                            }
                          , myData =
                            { myIndex = myIndex
                            , myCards = myCards
                            }
                          }
                          allPlayerIndices
                      , Nothing
                      )
              
              _ ->
                fail "Did not receive game data"

    , fuzz3 maximumBidFuzzer (commonDataFuzzer (intRange 150 250)) (list playerIndexFuzzer)
        "updates the maximum bid correctly" <|
          \maximumBid commonData bidders ->
            case maximumBid of
              MaximumBid bidder bid ->
                case handleReceivedMessages maximumBid (BiddingRound commonData bidders) of
                  stateAndCommand ->
                    let
                      newBiddingData biddingData =
                        { biddingData
                        | highestBid = bid
                        , highestBidder = bidder
                        }
                    in
                    equal
                      stateAndCommand
                      ( BiddingRound { commonData | biddingData = newBiddingData commonData.biddingData } bidders
                      , Nothing
                      )

              _ ->
                fail "Did not receive maximum data"

    , fuzz2 playerIndexFuzzer (commonDataFuzzer (intRange 150 250))
        "removes player from bidders when they quit bidding" <|
          \quitter commonData ->
            case handleReceivedMessages (HasQuitBidding quitter) (BiddingRound commonData allPlayerIndices) of
              stateAndCommand ->
                equal
                  stateAndCommand
                  ( BiddingRound commonData <| List.filter ((/=) quitter) allPlayerIndices
                  , Nothing
                  )

    , fuzz2 playerIndexFuzzer (commonDataFuzzer (intRange 150 250))
        "updates state to Trump selection when last player quits, I won bidding" <|
          \quitter commonData ->
            let
              newCommonData =
                { commonData | biddingData = newBiddingData commonData.biddingData }
              newBiddingData biddingData = { biddingData | highestBidder = commonData.myData.myIndex }
            in
            case handleReceivedMessages (HasQuitBidding quitter) (BiddingRound newCommonData [quitter]) of
              stateAndCommand ->
                equal
                  stateAndCommand
                  ( TrumpSelection newCommonData
                      { trump = Spade
                      , helpers = []
                      }
                  , Nothing
                  )

    , fuzz2 playerIndexFuzzer (commonDataFuzzer (intRange 150 250))
        "updates state to Waiting for trump when last player quits, someone else won bidding" <|
          \quitter commonData ->
            let
              newCommonData =
                { commonData | biddingData = newBiddingData commonData.biddingData }
              newBiddingData biddingData = { biddingData | highestBidder = nextTurn commonData.myData.myIndex }
            in
            case handleReceivedMessages (HasQuitBidding quitter) (BiddingRound newCommonData [quitter]) of
              stateAndCommand ->
                equal
                  stateAndCommand
                  ( WaitingForTrump newCommonData
                  , Nothing
                  )
    ]