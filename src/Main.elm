module Main exposing(..)


-- External imports
import Browser


-- Local imports
import Model exposing (initModel)
import Subscriptions exposing (subscriptions)
import Update exposing (update)
import View exposing (view)


--MAIN
main =
  Browser.element
    { init = initModel
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
