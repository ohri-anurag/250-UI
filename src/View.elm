module View exposing (..)


import Html exposing (Attribute, Html, button, div, img, input, label, option, select, span, text)
import Html.Attributes exposing (attribute, height, src, style, width)
import Html.Events exposing (on, onClick, onInput)
import String exposing (fromInt)


import Model exposing (..)


view : Model -> Html Msg
view model =
  case model of
    BeginGamePage playerName gameName ->
      beginGamePageView playerName gameName

    WaitingForPlayers playerNames gameName ->
      waitingForPlayersView playerNames gameName

    BiddingRound gameName biddingRoundData ->
      let
        me = getPlayer biddingRoundData.playerSet biddingRoundData.myData.myIndex
      in
      div
        [attribute "class" "biddingRoundView"]
        [ div
            [attribute "class" "biddingRoundSidebar"]
            [ gameNameView gameName
            , biddingZoneView biddingRoundData
            ]
        , div
            [attribute "class" "biddingRoundContent"]
            [ List.map (\i -> (i, Undecided)) allPlayerIndices
              |> otherPlayersView biddingRoundData.myData.myIndex biddingRoundData.playerSet
            , div [attribute "class" "filler"] []
            , myCardsView RoundFinished biddingRoundData.myData.myCards me
            ]
        ]

    TrumpSelection gameName trumpSelectionData ->
      trumpSelectionView trumpSelectionData

    WaitingForTrump gameName biddingRoundData ->
      div
        [attribute "class" "waitingForTrumpView"]
        [ "Waiting for "
            ++ (getPlayer biddingRoundData.playerSet biddingRoundData.biddingData.highestBidder).name
            ++ " to select trump. Bid Amount: "
            ++ fromInt biddingRoundData.biddingData.highestBid
          |> text
        ]

    PlayRound gameName playRoundData ->
      let
        myIndex = playRoundData.myData.myIndex
        me = getPlayer playRoundData.playerSet myIndex
      in
      div
        [attribute "class" "playRoundView"]
        [ div
            [attribute "class" "playRoundSidebar"]
            [ gameNameView gameName
            , staticInfoView playRoundData playRoundData.turnStatus playRoundData.roundIndex
            ]
        , div
            [attribute "class" "playRoundContent"]
            [ getPlayerStatuses playRoundData.playersStatus
              |> otherPlayersView myIndex playRoundData.playerSet
            , playAreaView playRoundData.hand myIndex
            , myCardsView playRoundData.turnStatus playRoundData.myData.myCards me
            ]
        ]


beginGamePageView : String -> String -> Html Msg
beginGamePageView playerName gameName =
  div
    [attribute "class" "beginGame"]
    [ div
        [attribute "class" "beginGameHeader"]
        [text "Welcome to the card game 250!!"]
    , div []
        [ label [attribute "for" ""] [text "Enter your display name:"]
        , input [onInput UpdatePlayerName] [text playerName]
        ]
    , div []
        [ label [attribute "for" ""] [text "Enter a name for the group:"]
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


biddingZoneView : BiddingRoundData -> Html Msg
biddingZoneView biddingRoundData =
  let
    highestBidderName =
      if biddingRoundData.biddingData.highestBidder == biddingRoundData.myData.myIndex
        then "You"
        else
          getPlayer biddingRoundData.playerSet biddingRoundData.biddingData.highestBidder
          |> .name
    bidderNames = List.map (getPlayer biddingRoundData.playerSet >> .name) biddingRoundData.bidders
    biddingHtml =
      if biddingRoundData.amIBidding
        then
          [ div
              [attribute "class" "bidButtonContainer"]
              [ button
                  [ attribute "class" "bidButton"
                  , onClick BidPlus5
                  ]
                  [text "+5"]
              , button
                  [ attribute "class" "bidButton"
                  , onClick BidPlus10
                  ]
                  [text "+10"]
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
    bidders =
      let bidder name = span [] [text name]
      in
      List.map bidder bidderNames
  in
  [ span
      [attribute "class" "bidValueLabel"]
      [text "Highest Bid"]
  , span
      [attribute "class" "bidValue"]
      [fromInt biddingRoundData.biddingData.highestBid |> text]
  , span []
      ["(" ++ highestBidderName ++ ")" |> text]
  ]
  ++
  biddingHtml
  ++
  [ span []
      [text "Current Bidders"]
    ::
    bidders
    |> span
        [attribute "class" "bidders"]
  ]
  |> div
      [ attribute "class" "biddingZone" ]



trumpSelectionView : TrumpSelectionData -> Html Msg
trumpSelectionView trumpSelectionData =
  let
    me = getPlayer trumpSelectionData.playerSet trumpSelectionData.myData.myIndex

    trumpView suit =
      let
        isSelected = suit == trumpSelectionData.selectionData.trump

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

    filteredCards = List.filter (\card -> List.member card trumpSelectionData.myData.myCards |> not) allCards

    helperCardAttrList card =
      [ SelectHelper card
        |> onClick
      ]
      ++
      if isPlayerHelper card trumpSelectionData.selectionData
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
    , myCardsView RoundFinished trumpSelectionData.myData.myCards me
    ]


staticInfoView : PlayRoundData -> TurnStatus -> Round -> Html Msg
staticInfoView playRoundData turnStatus round =
  let
    biddingInfoView =
      div
        [ attribute "class" "biddingInfo" ]
        [ span
            [attribute "class" "bidValueLabel"]
            [pronounify playRoundData.biddingData.highestBidder ++ " bid" |> text]
        , span
            [attribute "class" "bidValue"]
            [fromInt playRoundData.biddingData.highestBid |> text]
        ]

    pronounify playerIndex =
      if playerIndex == playRoundData.myData.myIndex
        then "Your"
        else
          getPlayer playRoundData.playerSet playerIndex
          |> .name
            |> \n -> n ++ "'s"

    helperView helper =
      helper
      |> cardView []

    helpers =
      List.map helperView playRoundData.selectionData.helpers

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
          pronounify playRoundData.myData.myIndex ++ " Turn"


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
        , Card Ace playRoundData.selectionData.trump |> cardView []
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


playAreaView : Hand -> PlayerIndex -> Html Msg
playAreaView hand myIndex =
  let
    otherPlayers = rotateOtherPlayers allPlayerIndices

    rotateOtherPlayers allPlayers =
      case allPlayers of
        (x :: xs) ->
          if x == myIndex
            then xs
            else xs ++ [x] |> rotateOtherPlayers

        [] ->
          []

    playerCardView i playerIndex =
      let
        defaultView =
          div ["yetToPlay p" ++ String.fromInt i |> attribute "class"]
            [span [] [text "Yet to play"]]
      in
      getCardFromHand playerIndex hand
      |> Maybe.map (cardView ["playerCard p" ++ String.fromInt i |> attribute "class" ])
      |> Maybe.withDefault defaultView

    playerCards =
      List.indexedMap playerCardView otherPlayers

    myCard = playerCardView 5 myIndex
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
