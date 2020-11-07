module View exposing (..)


import Html exposing (Attribute, Html, a, button, div, h1, img, input, label, option, select, span, text)
import Html.Attributes exposing (attribute, height, href, src, style, value, width)
import Html.Events exposing (on, onClick, onInput)
import String exposing (fromInt)


import Model.Card exposing (..)
import Model exposing (..)
import View.Analytics exposing (analyticsPageView)


view : Model -> Html Msg
view model =
  case model of
    BeginGamePage playerId playerName gameName validation ->
      beginGamePageView playerId playerName gameName validation

    AnalyticsPage analyticsMode ->
      analyticsPageView analyticsMode

    WaitingForServerValidation playerId playerName gameName ->
      div
        [ attribute "class" "serverValidationContainer" ]
        [ div []
          [ text "Waiting for server to validate your username and player name" ]
        ]

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
              |> otherPlayersView commonData.myData.myIndex commonData.playerSet bidders
            , div [attribute "class" "filler"] []
            , myCardsView RoundFinished commonData.myData.myCards me
            ]
        ]

    TrumpSelection commonData selectionData ->
      trumpSelectionView commonData selectionData

    WaitingForTrump commonData ->
      div []
        [ "Waiting for "
            ++ (getPlayer commonData.playerSet commonData.biddingData.highestBidder).name
            ++ " to select trump. Bid Amount: "
            ++ fromInt commonData.biddingData.highestBid
          |> text
        ]
      |> List.singleton
      |> div [attribute "class" "waitingForTrumpView"]

    PlayRound commonData playRoundData ->
      let
        myIndex = commonData.myData.myIndex
        me = getPlayer commonData.playerSet myIndex
        playerCards =
          getPlayers .card commonData.playerSet
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
            [ getPlayers .status commonData.playerSet
              |> otherPlayersView myIndex commonData.playerSet []
            , playAreaView playerCards myIndex
            , myCardsView playRoundData.turnStatus commonData.myData.myCards me
            ]
        ]

    ErrorState ->
      div
        [attribute "class" "errorContainer"]
        [ div
            [attribute "class" "error"]
            [ h1
                [attribute "class" "errorHeading"]
                [text "OOPS!!"]
            , div
                [attribute "class" "errorPara"]
                [text
                  """
                  No, I'm not talking about "Object Oriented Programming Systems".
                  Your websocket connection has failed.


                  But no need to worry dear friend!!
                  Just refresh your page and re-enter your details,
                  specifically your id.

                  You'll magically re-join the game,
                  much as your websocket magically got disconnected.
                  """]
            ]
        ]


beginGamePageView : String -> String -> String -> Maybe Validation -> Html Msg
beginGamePageView playerId playerName gameName validation =
  let
    errorText =
      case validation of
        Just error ->
          case error of
            EmptyId ->
              "Username can't be empty"

            EmptyName ->
              "Player Name can't be empty"

            EmptyGameName ->
              "Game Name can't be empty"

            DuplicateId ->
              "A Player with the same username already exists. Please choose another."

            DuplicateName ->
              "A Player with same player name already exists. Please choose another."

        Nothing ->
          ""
  in
  div [attribute "class" "beginGameContainer"] <|
    [ div
      [attribute "class" "beginGame"]
      [ div
          [attribute "class" "beginGameHeader"]
          [text "Welcome to 250 - SEASON 1!!"]
      , div
          [attribute "class" "beginGameInputs"]
          [ label [] [text "Enter your username (this will be used if you get disconnected):"]
          , input
              ( [ value playerId
                , onInput UpdatePlayerId
                ]
                ++
                if validation == Just EmptyId then [attribute "class" "errorInput"] else []
              )
              [text playerId]
          ]
      , div
          [attribute "class" "beginGameInputs"]
          [ label [] [text "Enter your player name (used for display):"]
          , input
              ( [ value playerName
                , onInput UpdatePlayerName
                ]
                ++
                if validation == Just EmptyName then [attribute "class" "errorInput"] else []
              )
              [text playerName]
          ]
      , div
          [attribute "class" "beginGameInputs"]
          [ label [] [text "Enter a name for the group:"]
          , input
              ( [ value gameName
                , onInput UpdateGameName
                ]
                ++
                if validation == Just EmptyGameName then [attribute "class" "errorInput"] else []
              )
              [text gameName]
          ]
      , button
          [ attribute "class" "beginGameButton"
          , onClick SendGameName
          ]
          [text "Begin Game"]
      ]
    , div
        [attribute "class" "errorView"]
        [ text errorText ]
    , button
        [ attribute "class" "analytics"
        , onClick AnalyticsClicked
        ]
        [text "Scorecard - Season 1"]
    , a
        [attribute "class" "help", href "/help.html"]
        [text "How It Works"]
    ]


waitingForPlayersView : List String -> String -> Html Msg
waitingForPlayersView playerNames gameName =
  let
    player name = div [] [text name]
    players = List.map player playerNames
  in
  [ div
      [ attribute "class" "waitingForPlayersHeader" ]
      [ text <| "Waiting For 6 Players in " ++ gameName ]
  , div []
      [ text "Current Players:" ]
  ]
  ++ players
  |> div []
  |> List.singleton
  |> div [ attribute "class" "waitingForPlayersView" ]


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


otherPlayersView : PlayerIndex -> PlayerSet -> List PlayerIndex -> List (PlayerIndex, PlayerStatus) -> Html Msg
otherPlayersView myIndex playerSet bidders allStatuses =
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
      |> List.map (\(i, s) -> (i, getPlayer playerSet i, isAllied s))
  in
  otherPlayers
    |> List.indexedMap (playerView bidders)
    |> div
        [ attribute "class" "players" ]
    |> List.singleton
    |> div
        [ attribute "class" "playersContainer" ]


biddingZoneView : CommonData -> List PlayerIndex -> Html Msg
biddingZoneView commonData bidders =
  let
    firstTurnName =
      if commonData.biddingData.firstBidder == commonData.myData.myIndex
        then "You"
        else
          getPlayer commonData.playerSet commonData.biddingData.firstBidder
          |> .name
    highestBidderName =
      if commonData.biddingData.highestBidder == commonData.myData.myIndex
        then "You"
        else
          getPlayer commonData.playerSet commonData.biddingData.highestBidder
          |> .name
    bidderNames = List.map (getPlayer commonData.playerSet >> .name) bidders
    buttons =
      div
        [ attribute "class" "bidButton"
        , onClick BidPlus5
        ]
        [text "+5"]
      ::
      if commonData.biddingData.highestBid > 240
        then
        []
        else
        [ div
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
              , div
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
      [attribute "class" "firstTurnLabel"]
      [text "First Turn"]
  , span
      [attribute "class" "firstTurn"]
      ["(" ++ firstTurnName ++ ")" |> text]
  , span
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
  -- ++
  -- [ span []
  --     [text "Current Bidders"]
  --   ::
  --   bidderDivs
  --   |> span
  --       [attribute "class" "bidders"]
  -- ]
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
        , div
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


playerView : List PlayerIndex -> Int -> (PlayerIndex, Player, Maybe Bool) -> Html Msg
playerView bidders i (playerIndex, player, maybeIsAllied) =
  let
    alliedClass =
      case maybeIsAllied of
        Just isAllied ->
          if isAllied
            then "ally "
            else "enemy "

        Nothing ->
          ""

    isBidding = List.member playerIndex bidders

    biddingClass =
      if isBidding
        then "bidder" ++ fromInt i ++ " "
        else ""
  in
  div
    [ biddingClass ++ alliedClass ++ "player p" ++ fromInt i |> attribute "class" ]
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
