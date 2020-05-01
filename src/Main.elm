module Main exposing(..)


-- External imports
import Browser


-- Local imports
import Model exposing (initModel)
import Subscriptions exposing (subscriptions)
import Update exposing (update)
import View exposing (view)



-- forEachPlayer : (PlayerIndex -> a) -> List a
-- forEachPlayer fn =
--   [ fn Player1
--   , fn Player2
--   , fn Player3
--   , fn Player4
--   , fn Player5
--   , fn Player6
--   ]

--MAIN
main =
  Browser.element
    { init = initModel
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
