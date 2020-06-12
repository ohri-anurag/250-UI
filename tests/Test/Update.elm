module Test.Update exposing (..)

import Expect exposing (..)
-- import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tuple exposing (first, second)


import Model exposing (..)
import Update exposing (..)


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

    -- , let
    --     commonData = 
    --       { gameName = ""
    --       , playerSet = ""
    --       , biddingData = ""
    --       , myData = ""
    --       }
    --   testStateAndCommand
    --     "updates the bid in Bidding Round properly"
    ]