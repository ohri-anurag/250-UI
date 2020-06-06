module View exposing (..)


import Html exposing (Attribute, Html, button, div, img, input, label, option, select, span, text)
import Html.Attributes exposing (attribute, height, src, style, width)
import Html.Events exposing (on, onClick, onInput)
import String exposing (fromInt)


import Model.Card exposing (..)
import Model exposing (..)


view : Model -> Html Msg
view model =
  case model of
    BeginGamePage playerId playerName gameName ->
      beginGamePageView playerId playerName gameName

    WaitingForPlayers playerNames gameName ->
      waitingForPlayersView playerNames gameName

    BiddingRound commonData bidders ->
      let
        me = getPlayer commonData.playerSet commonData.myData.myIndex
      in
      div
        [attribute "class" "biddingRoundView"]
        [ div
            [attribute "class" "biddingRoundSidebar"]
            [ gameNameView commonData.gameName
            , biddingZoneView commonData bidders
            ]
        , div
            [attribute "class" "biddingRoundContent"]
            [ List.map (\i -> (i, Undecided)) allPlayerIndices
              |> otherPlayersView commonData.myData.myIndex commonData.playerSet
            , div [attribute "class" "filler"] []
            , myCardsView RoundFinished commonData.myData.myCards me
            ]
        ]

    TrumpSelection commonData selectionData ->
      trumpSelectionView commonData selectionData

    WaitingForTrump commonData ->
      div
        [attribute "class" "waitingForTrumpView"]
        [ "Waiting for "
            ++ (getPlayer commonData.playerSet commonData.biddingData.highestBidder).name
            ++ " to select trump. Bid Amount: "
            ++ fromInt commonData.biddingData.highestBid
          |> text
        ]

    PlayRound commonData playRoundData ->
      let
        myIndex = commonData.myData.myIndex
        me = getPlayer commonData.playerSet myIndex
        playerCards =
          getPlayers commonData.playerSet
          |> List.map .card
          |> List.map2 Tuple.pair allPlayerIndices
      in
      div
        [attribute "class" "playRoundView"]
        [ div
            [attribute "class" "playRoundSidebar"]
            [ gameNameView commonData.gameName
            , staticInfoView commonData playRoundData.selectionData playRoundData.turnStatus playRoundData.roundIndex
            ]
        , div
            [attribute "class" "playRoundContent"]
            [ getPlayerStatuses playRoundData.playersStatus
              |> otherPlayersView myIndex commonData.playerSet
            , playAreaView playerCards myIndex
            , myCardsView playRoundData.turnStatus commonData.myData.myCards me
            ]
        ]


beginGamePageView : String -> String -> String -> Html Msg
beginGamePageView playerId playerName gameName =
  div
    [attribute "class" "beginGame"]
    [ div
        [attribute "class" "beginGameHeader"]
        [text "Welcome to the card game 250!!"]
    , div []
        [ label [] [text "Enter your id(this will be used if you get disconnected):"]
        , input [onInput UpdatePlayerId] [text playerId]
        ]
    , div []
        [ label [] [text "Enter your display name:"]
        , input [onInput UpdatePlayerName] [text playerName]
        ]
    , div []
        [ label [] [text "Enter a name for the group:"]
        , input [onInput UpdateGameName] [text gameName]
        ]
    , div
        [ attribute "class" "beginGameButton"
        , onClick SendGameName
        ]
        [text "Begin Game"]
    ]


waitingForPlayersView : List String -> String -> Html Msg
waitingForPlayersView playerNames gameName =
  let
    player name = div [] [text name]
    players = List.map player playerNames
  in
  [ div
      [attribute "class" "waitingForPlayersHeader"]
      [ "Waiting For 6 Players in " ++ gameName
        |> text
      ]
  , div
      []
      [text "Current Players:"]
  ]
  ++ players
  |> div
      [attribute "class" "waitingForPlayers"]


myCardsView : TurnStatus -> List Card -> Player -> Html Msg
myCardsView turnStatus myCards me =
  let
    cardList =
      let
        attrList card =
          case turnStatus of
            FirstAndMyTurn ->
              [SendCard card |> onClick]
            NotFirstAndMyTurn baseCard ->
              let
                hasValidCard =
                  List.any (\c -> c.suit == baseCard.suit) myCards
              in
              if hasValidCard && card.suit /= baseCard.suit
                then [attribute "class" "blurCard"]
                else [SendCard card |> onClick]

            _ ->
              []
      in
      List.map (\card -> cardView (attrList card) card) myCards

  in
  div
    [ attribute "class" "myCardsContainer" ]
    [ div
        [ attribute "class" "myName" ]
        [ text "Your cards" ]
    , cardList
      |> div
        [ attribute "class" "myCards" ]
    , div
        [ attribute "class" "myScores" ]
        [ div [] [ "This game's score: " ++ fromInt me.gameScore |> text ]
        , div [] [ "Total Score: " ++ fromInt me.totalScore |> text ]
        ]
    ]
  


gameNameView : String -> Html Msg
gameNameView name =
  div
    [ attribute "class" "gameName" ]
    [ text name ]


otherPlayersView : PlayerIndex -> PlayerSet -> List (PlayerIndex, PlayerStatus) -> Html Msg
otherPlayersView myIndex playerSet allStatuses =
  let
    myStatus =
      lookup myIndex allStatuses
      |> Maybe.withDefault Undecided

    isAllied playerStatus =
      -- If my own status is undecided(bidding is going on), then I don't know anyone else's status, return nothing
      -- If a player's status is undecided, also return nothing
      if myStatus == Undecided || playerStatus == Undecided
        then Nothing
        -- Otherwise, a player is allied to me, if their status is same as mine.
        else playerStatus == myStatus |> Just

    rotateOtherPlayers allPlayers =
      case allPlayers of
        (x :: xs) ->
          if Tuple.first x == myIndex
            then xs
            else xs ++ [x] |> rotateOtherPlayers

        [] ->
          []

    otherPlayers =
      rotateOtherPlayers allStatuses
      |> List.map (Tuple.mapBoth (getPlayer playerSet) isAllied)
  in
  otherPlayers
    |> List.indexedMap playerView
    |> div
        [ attribute "class" "players" ]
    |> List.singleton
    |> div
        [ attribute "class" "playersContainer" ]


biddingZoneView : CommonData -> List PlayerIndex -> Html Msg
biddingZoneView commonData bidders =
  let
    highestBidderName =
      if commonData.biddingData.highestBidder == commonData.myData.myIndex
        then "You"
        else
          getPlayer commonData.playerSet commonData.biddingData.highestBidder
          |> .name
    bidderNames = List.map (getPlayer commonData.playerSet >> .name) bidders
    buttons =
      button
        [ attribute "class" "bidButton"
        , onClick BidPlus5
        ]
        [text "+5"]
      ::
      if commonData.biddingData.highestBid > 240
        then
        []
        else
        [ button
            [ attribute "class" "bidButton"
            , onClick BidPlus10
            ]
            [text "+10"]
        ]
    biddingHtml =
      if List.member commonData.myData.myIndex bidders
        then
          [ div
              [attribute "class" "bidButtonContainer"]
              [ div
                  [attribute "class" "increaseBidButtons"]
                  buttons
              , button
                  [ attribute "class" "quitBiddingButton"
                  , onClick QuitBidding
                  ]
                  [text "Quit Bidding"]
              ]
          ]
        else
          [ div
              [attribute "class" "bidButtonContainer"]
              [text "You can't bid anymore."]
          ]
    bidderDivs =
      let bidder name = span [] [text name]
      in
      List.map bidder bidderNames
  in
  [ span
      [attribute "class" "bidValueLabel"]
      [text "Highest Bid"]
  , span
      [attribute "class" "bidValue"]
      [fromInt commonData.biddingData.highestBid |> text]
  , span []
      ["(" ++ highestBidderName ++ ")" |> text]
  ]
  ++
  biddingHtml
  ++
  [ span []
      [text "Current Bidders"]
    ::
    bidderDivs
    |> span
        [attribute "class" "bidders"]
  ]
  |> div
      [ attribute "class" "biddingZone" ]



trumpSelectionView : CommonData -> SelectionData -> Html Msg
trumpSelectionView commonData selectionData =
  let
    me = getPlayer commonData.playerSet commonData.myData.myIndex

    trumpView suit =
      let
        isSelected = suit == selectionData.trump

        attrList = [SelectTrump suit |> onClick] ++
          if isSelected
            then [attribute "class" "selectedTrump"]
            else [attribute "class" "trump"]
        labelAttr =
          if isSelected
            then " selectedLabel"
            else ""

      in

      div
        [attribute "class" "trumpWithLabel"]
        [ div
            [ "trumpLabel" ++ labelAttr
              |> attribute "class"
            ]
            [ showSuit True suit
              |> text
            ]
        , Card Ace suit
          |> cardView attrList
        ]

    filteredCards = List.filter (\card -> List.member card commonData.myData.myCards |> not) allCards

    helperCardAttrList card =
      [ SelectHelper card
        |> onClick
      ]
      ++
      if isPlayerHelper card selectionData
        then [attribute "class" "selectedHelper"]
        else [attribute "class" "helper"]

    helperCards =
      List.map (\card ->
        cardView
          ( helperCardAttrList card )
          card
      ) filteredCards

  in

  div
    [attribute "class" "trumpContainer"]
    [ div
        [attribute "class" "trumpBox"]
        [ span
            [attribute "class" "trumpBoxHeader"]
            [text "Select Trump"]
        , div
            [attribute "class" "trumps"]
            [ trumpView Spade
            , trumpView Heart
            , trumpView Club
            , trumpView Diamond
            ]
        , div
            [attribute "class" "helperHeader"]
            [text "Select Helpers"]
          ::
          helperCards
          |> div [attribute "class" "helperContainer"]
        , button
          [attribute "class" "proceedButton"
          , onClick SendTrump
          ]
          [text "Proceed"]
        ]
    , myCardsView RoundFinished commonData.myData.myCards me
    ]


staticInfoView : CommonData -> SelectionData -> TurnStatus -> Round -> Html Msg
staticInfoView commonData selectionData turnStatus round =
  let
    biddingInfoView =
      div
        [ attribute "class" "biddingInfo" ]
        [ span
            [attribute "class" "bidValueLabel"]
            [pronounify commonData.biddingData.highestBidder ++ " bid" |> text]
        , span
            [attribute "class" "bidValue"]
            [fromInt commonData.biddingData.highestBid |> text]
        ]

    pronounify playerIndex =
      if playerIndex == commonData.myData.myIndex
        then "Your"
        else
          getPlayer commonData.playerSet playerIndex
          |> .name
            |> \n -> n ++ "'s"

    helperView helper =
      helper
      |> cardView []

    helpers =
      List.map helperView selectionData.helpers

    turnView =
      case turnStatus of
        FirstAndNotMyTurn player ->
          pronounify player ++ " Turn"

        NotFirstAndNotMyTurn player _ ->
          pronounify player ++ " Turn"

        RoundFinished ->
          "Waiting for round to finish.."

        GameFinished ->
          "Waiting for game to finish.."

        _ ->
          pronounify commonData.myData.myIndex ++ " Turn"


    roundView =
      case turnStatus of
        GameFinished ->
          ""

        _ ->
          showRound False round


  in
  div
    [attribute "class" "miniTrumpAndHelper"]
    [ biddingInfoView
    , div 
        [attribute "class" "miniTrump"]
        [ span [] [text "Trump"]
        , Card Ace selectionData.trump |> cardView []
        ]
    , div
        [attribute "class" "miniHelper"]
        [ span [] [text "Helpers"]
        , span []
            helpers
        ]
    , div
        [attribute "class" "round"]
        [ span [] [text roundView] ]
    , div
        [attribute "class" "turn"]
        [ span [] [text turnView] ]
    ]


playAreaView : List (PlayerIndex, Maybe Card) -> PlayerIndex -> Html Msg
playAreaView cards myIndex =
  let
    (otherPlayers, me) = rotateOtherPlayers cards

    rotateOtherPlayers allPlayers =
      case allPlayers of
        (x :: xs) ->
          if Tuple.first x == myIndex
            then (xs, x)
            else xs ++ [x] |> rotateOtherPlayers

        [] ->
          ([], (myIndex, Nothing))

    playerCardView i (playerIndex, card) =
      let
        defaultView =
          div ["yetToPlay p" ++ String.fromInt i |> attribute "class"]
            [span [] [text "Yet to play"]]
      in
      Maybe.map (cardView ["playerCard p" ++ String.fromInt i |> attribute "class" ]) card
      |> Maybe.withDefault defaultView

    playerCards =
      List.indexedMap playerCardView otherPlayers

    myCard = playerCardView 5 me
  in
  div
    [attribute "class" "playArea"]
    [ div
        [attribute "class" "playerCards"]
        playerCards
    , myCard
    ]


playerView : Int -> (Player, Maybe Bool) -> Html Msg
playerView i (player, maybeIsAllied) =
  let
    alliedClass =
      case maybeIsAllied of
        Just isAllied ->
          if isAllied
            then "ally"
            else "enemy"

        Nothing ->
          ""
  in
  div
    [ alliedClass ++ " player p" ++ fromInt i |> attribute "class" ]
    [ player.name
        |> text
        |> List.singleton
        |> span [ attribute "class" "playerName" ]
    , span
        [ attribute "class" "playerScore" ]
        [ fromInt player.totalScore |> text ]
    , span
        [ attribute "class" "playerScoreLabel" ]
        [ text "Total" ]
    , span
        [ attribute "class" "playerScore" ]
        [ fromInt player.gameScore |> text ]
    , span
        [ attribute "class" "playerScoreLabel" ]
        [ text "Current score" ]
    ]


cardView : List (Attribute Msg) -> Card -> Html Msg
cardView attrList card =
  let
    path = "img/" ++ showCardValue card.value ++ " of " ++ showSuit True card.suit ++ ".png"
    attributeList = [ src path ] ++ attrList
  in
  img attributeList []
