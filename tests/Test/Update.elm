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
        (BeginGamePage "" "" "")
        (BeginGamePage "" "" "test", Nothing)

    , testStateAndCommand
        "updates player name properly"
        (UpdatePlayerName "test")
        (BeginGamePage "" "" "")
        (BeginGamePage "" "test" "", Nothing)

    , testStateAndCommand
        "updates player id properly"
        (UpdatePlayerId "test")
        (BeginGamePage "" "" "")
        (BeginGamePage "test" "" "", Nothing)

    , testStateAndCommand
        "changes state to WaitingForPlayers when player clicks on Begin game"
        SendGameName
        (BeginGamePage "a" "b" "c")
        (WaitingForPlayers ["b"] "c", IntroData  "a" "b" "c" |> Just)
    ]