port module Update exposing (..)


import Model exposing (..)


port sendMessage : String -> Cmd msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateGameName str ->
      (BeginGamePage str, Cmd.none)

    SendGameName ->
      case model of
        BeginGamePage str ->
          (WaitingForPlayers, sendMessage str)

        _ ->
          (model, Cmd.none)

    BeginGame index ->
      (initGameState index |> GameState, Cmd.none)
    
    _ ->
      (model, Cmd.none)