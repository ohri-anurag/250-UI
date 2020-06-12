module Test.Update exposing (..)

import Expect exposing (..)
-- import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tuple exposing (first, second)


import Model.Card exposing (..)
import Model exposing (..)
import Update exposing (..)


testPlayer : PlayerIndex -> Player
testPlayer playerIndex =
  { totalScore = 0
  , gameScore = 0
  , name = showPlayerIndex playerIndex
  , card = Nothing
  , status = Undecided
  }


testCommonData : CommonData
testCommonData =
  { gameName = "g"
  , playerSet =
    { player1 = testPlayer Player1
    , player2 = testPlayer Player2
    , player3 = testPlayer Player3
    , player4 = testPlayer Player4
    , player5 = testPlayer Player5
    , player6 = testPlayer Player6
    }
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


changeBid : Int -> CommonData
changeBid b =
  { gameName = "g"
  , playerSet =
    { player1 = testPlayer Player1
    , player2 = testPlayer Player2
    , player3 = testPlayer Player3
    , player4 = testPlayer Player4
    , player5 = testPlayer Player5
    , player6 = testPlayer Player6
    }
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


suite : Test
suite =
  describe "Update function"
    [ testStateAndCommand
        "updates game name properly"
        (UpdateGameName "test")
        (BeginGamePage "" "" "" Nothing)
        (BeginGamePage "" "" "test" Nothing, Nothing)

    , testStateAndCommand
        "updates player name properly"
        (UpdatePlayerName "test")
        (BeginGamePage "" "" "" Nothing)
        (BeginGamePage "" "test" "" Nothing, Nothing)

    , testStateAndCommand
        "updates player id properly"
        (UpdatePlayerId "test")
        (BeginGamePage "" "" "" Nothing)
        (BeginGamePage "test" "" "" Nothing, Nothing)

    , testStateAndCommand
        "changes state to WaitingForServerValidation when player clicks on Begin game with valid id, name and game"
        SendGameName
        (BeginGamePage "a" "b" "c" Nothing)
        (WaitingForServerValidation "a" "b" "c", IntroData  "a" "b" "c" |> Just)

    , testStateAndCommand
        "changes state to BeginGamePage when player clicks on Begin game with invalid id"
        SendGameName
        (BeginGamePage "" "b" "c" Nothing)
        (BeginGamePage "" "b" "c" <| Just EmptyId, Nothing)

    , testStateAndCommand
        "changes state to WaitingForPlayers when player clicks on Begin game with invalid name"
        SendGameName
        (BeginGamePage "a" "" "c" Nothing)
        (BeginGamePage "a" "" "c" <| Just EmptyName, Nothing)

    , testStateAndCommand
        "changes state to WaitingForPlayers when player clicks on Begin game with invalid game"
        SendGameName
        (BeginGamePage "a" "b" "" Nothing)
        (BeginGamePage "a" "b" "" <| Just EmptyGameName, Nothing)

    , testStateAndCommand
        "updates the bid in Bidding Round properly by 5 (new bid less than 250)"
        BidPlus5
        (BiddingRound testCommonData [])
        ( BiddingRound testCommonData []
        , testCommonData.biddingData.highestBid + 5
          |> IncreaseBid testCommonData.gameName testCommonData.myData.myIndex
          |> Just
        )
    , let
        newCommonData = changeBid 245
      in
      testStateAndCommand
        "updates the bid in Bidding Round properly by 5 (new bid equal to 250)"
        BidPlus5
        (BiddingRound newCommonData [])
        ( BiddingRound newCommonData []
        , IncreaseBid newCommonData.gameName newCommonData.myData.myIndex 250
          |> Just
        )
    , let
        newCommonData = changeBid 250
      in
      testStateAndCommand
        "updates the bid in Bidding Round properly by 5 (new bid greater than 250)"
        BidPlus5
        (BiddingRound newCommonData [])
        ( BiddingRound newCommonData []
        , IncreaseBid newCommonData.gameName newCommonData.myData.myIndex 250
          |> Just
        )
    , testStateAndCommand
        "updates the bid in Bidding Round properly by 10 (new bid less than 250)"
        BidPlus10
        (BiddingRound testCommonData [])
        ( BiddingRound testCommonData []
        , testCommonData.biddingData.highestBid + 10
          |> IncreaseBid testCommonData.gameName testCommonData.myData.myIndex
          |> Just
        )
    , let
        newCommonData = changeBid 240
      in
      testStateAndCommand
        "updates the bid in Bidding Round properly by 10 (new bid equal to 250)"
        BidPlus10
        (BiddingRound newCommonData [])
        ( BiddingRound newCommonData []
        , IncreaseBid newCommonData.gameName newCommonData.myData.myIndex 250
          |> Just
        )
    , let
        newCommonData = changeBid 250
      in
      testStateAndCommand
        "updates the bid in Bidding Round properly by 10 (new bid greater than 250)"
        BidPlus10
        (BiddingRound newCommonData [])
        ( BiddingRound newCommonData []
        , IncreaseBid newCommonData.gameName newCommonData.myData.myIndex 250
          |> Just
        )
    , testStateAndCommand
        "removes me from bidders when I click Quit Bidding"
        QuitBidding
        (BiddingRound testCommonData allPlayerIndices)
        ( BiddingRound testCommonData <| List.filter ((/=) Player1) allPlayerIndices
        , SendQuit testCommonData.gameName testCommonData.myData.myIndex |> Just
        )
    , testStateAndCommand
        "selects trump when I click on it"
        (SelectTrump Diamond)
        (TrumpSelection testCommonData testSelectionData)
        ( TrumpSelection testCommonData <| { testSelectionData | trump = Diamond }
        , Nothing
        )
    ]