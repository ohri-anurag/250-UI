module View exposing (..)


import Html exposing (Attribute, Html, button, div, img, input, option, select, span, text)
import Html.Attributes exposing (attribute, height, src, style, width)
import Html.Events exposing (on, onClick, onInput)
import String exposing (fromInt)


import Model exposing (..)


view : Model -> Html Msg
view model =
  case model of
    BeginGamePage playerName gameName ->
      div []
        [ div [] [text "Welcome to the card game 250!!"]
        , div [] [text "Enter your display name:"]
        , input [onInput UpdatePlayerName] [text playerName]
        , div [] [text "Enter a name for the group:"]
        , input [onInput UpdateGameName] [text gameName]
        , button [onClick SendGameName] [text "Begin Game"]
        ]

    WaitingForPlayers ->
      div [] [text "Waiting For Players"]

    BiddingRound gameState bidders biddingData isBidding ->
      let
        me = getPlayer gameState.playerSet gameState.myIndex
        highestBidderName =
          if biddingData.highestBidder == gameState.myIndex
            then "You"
            else
              getPlayer gameState.playerSet biddingData.highestBidder
              |> .name
        bidderNames = List.map (getPlayer gameState.playerSet >> .name) bidders
      in
      div []
        [ gameNameView gameState.gameName
        , List.map (\i -> (i, Undecided)) allPlayerIndices
          |> otherPlayersView gameState
        , biddingZoneView highestBidderName bidderNames biddingData isBidding
        , Just me
          |> myCardsView Nothing (always []) gameState.myCards
        ]

    TrumpSelection selectionData _ gameState ->
      let
        me = getPlayer gameState.playerSet gameState.myIndex
      in
      trumpSelectionView selectionData gameState.myCards me

    WaitingForTrump _ _ ->
      div [] [text "Waiting for Bidding Winner to select trump"]

    PlayRound round playState isActive ->
      let
        me = getPlayer playState.gameState.playerSet playState.gameState.myIndex

        baseCard =
          -- If I am the first bidder, I have no restrictions
          if playState.firstPlayer == playState.gameState.myIndex
            then
              Nothing
            else
              -- If it is not my turn or I have already played a card, do not apply the blur effect
              if isActiveTurn
                then getCardFromHand playState.firstPlayer playState.hand
                else Nothing

        isActiveTurn = 
          Maybe.map (\t -> isActive && t == playState.gameState.myIndex) playState.turn
          |> Maybe.withDefault False

        attrList card =
          if isActiveTurn
            then [SendCard card |> onClick]
            else []
      in
      div []
        [ gameNameView playState.gameState.gameName
        , getPlayerStatuses playState.playersStatus
          |> otherPlayersView playState.gameState
        , staticInfoView playState round
        , playAreaView playState.hand playState.gameState.myIndex
        , Just me
          |> myCardsView baseCard attrList playState.gameState.myCards
        ]


myCardsView : Maybe Card -> (Card -> List (Attribute Msg)) -> List Card -> Maybe Player -> Html Msg
myCardsView maybeBaseCard oldAttrList myCards maybeMe =
  let
    scoreView = 
      case maybeMe of
        Just me ->
          [ div
            [ attribute "class" "myScores" ]
            [ div [] [ "This game's score: " ++ fromInt me.gameScore |> text ]
            , div [] [ "Total Score: " ++ fromInt me.totalScore |> text ]
            ]
          ]

        Nothing ->
          []

    cardList =
      let
        attrList card =
          case maybeBaseCard of
            Just baseCard ->
              let
                hasValidCard =
                  List.any (\c -> c.suit == baseCard.suit) myCards
              in
              if hasValidCard && card.suit /= baseCard.suit
                then [attribute "class" "blurCard"]
                else oldAttrList card

            Nothing ->
              oldAttrList card
      in
      List.map (\card -> cardView (attrList card) card) myCards

  in
  [ div
      [ attribute "class" "myName" ]
      [ text "Your cards" ]
    , cardList
      |> div
        [ attribute "class" "myCards" ]
  ]
  ++
  scoreView
  |> div
    [ attribute "class" "myCardsContainer" ]


gameNameView : String -> Html Msg
gameNameView name =
  div
    [ attribute "class" "gameName" ]
    [ text name ]


otherPlayersView : GameState -> List (PlayerIndex, PlayerStatus) -> Html Msg
otherPlayersView gameState allStatuses =
  let
    myStatus =
      lookup gameState.myIndex allStatuses
      |> Maybe.withDefault Undecided

    isAllied playerStatus =
      -- If my own status is undecided, then I don't know anyone else's status, return nothing
      -- If a player's status is undecided, also return nothing
      if myStatus == Undecided || playerStatus == Undecided
        then Nothing
        -- Otherwise, a player is allied to me, if their status is same as mine.
        else playerStatus == myStatus |> Just

    rotateOtherPlayers allPlayers =
      case allPlayers of
        (x :: xs) ->
          if Tuple.first x == gameState.myIndex
            then xs
            else xs ++ [x] |> rotateOtherPlayers

        [] ->
          []

    otherPlayers =
      rotateOtherPlayers allStatuses
      |> List.map (Tuple.mapBoth (getPlayer gameState.playerSet) isAllied)
  in
  otherPlayers
    |> List.indexedMap playerView
    |> div
        [ attribute "class" "players" ]
    |> List.singleton
    |> div
        [ attribute "class" "playersContainer" ]


biddingZoneView : String -> List String -> IBiddingData -> Bool -> Html Msg
biddingZoneView highestBidderName bidderNames biddingData isBidding =
  let
    biddingHtml =
      if isBidding
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
              ]
          , button
              [ attribute "class" "quitBiddingButton"
              , onClick QuitBidding
              ]
              [text "Quit Bidding"]
          ]
        else
          [ div [] [text "You can't bid anymore."]
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
      [fromInt biddingData.highestBid |> text]
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



trumpSelectionView : SelectionData -> List Card -> Player -> Html Msg
trumpSelectionView selectionData myCards me =
  let
    trumpView suit =
      let
        isSelected = suit == selectionData.selectedTrump

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

    filteredCards = List.filter (\card -> List.member card myCards |> not) allCards

    isHelperCard card = isHelper1 card || isHelper2 card

    isHelper1 card =
      case selectionData.helper1 of
        Just c1 ->
          card == c1

        Nothing ->
          False

    isHelper2 card =
      case selectionData.helper2 of
        Just c2 ->
          card == c2

        Nothing ->
          False

    helperCardAttrList card =
      [ SelectHelper card
        |> onClick
      ]
      ++
      if isHelperCard card
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
    , myCardsView Nothing (always []) myCards Nothing
    ]


staticInfoView : PlayState -> Round -> Html Msg
staticInfoView playState round =
  let
    biddingInfoView =
      div
        [ attribute "class" "biddingInfo" ]
        [ span
            [attribute "class" "bidValueLabel"]
            [pronounify playState.biddingData.biddingWinner ++ " bid" |> text]
        , span
            [attribute "class" "bidValue"]
            [fromInt playState.biddingData.winningBid |> text]
        ]

    pronounify playerIndex =
      if playerIndex == playState.gameState.myIndex
        then "Your"
        else
          getPlayer playState.gameState.playerSet playerIndex
          |> .name
            |> \n -> n ++ "'s"

    helper1View =
      playState.selectionData.helper1
      |> Maybe.map (cardView [] >> List.singleton)
      |> Maybe.withDefault []

    helper2View =
      playState.selectionData.helper2
      |> Maybe.map (cardView [] >> List.singleton)
      |> Maybe.withDefault []

    helpers =
      helper1View ++ helper2View

    turnView =
      Maybe.map (\t -> pronounify t ++ " Turn") playState.turn
      |> Maybe.withDefault "Waiting for round to finish.."

    roundView =
      showRound False round

  in
  div
    [attribute "class" "miniTrumpAndHelper"]
    [ biddingInfoView
    , div 
        [attribute "class" "miniTrump"]
        [ span [] [text "Trump"]
        , Card Ace playState.selectionData.selectedTrump |> cardView []
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
