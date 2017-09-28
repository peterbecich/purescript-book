module ComponentC where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Control.Monad.Aff (Aff)
--import Network.HTTP.Affjax (affjax, defaultRequest)
import Network.HTTP.Affjax as AX
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Data.HTTP.Method (Method(..))

type State = String

data Query a
  = HandleInput String a
  | GetValue (String -> a)

component :: forall m. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX, console :: CONSOLE | m))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = "Hello"

  render :: State -> H.ComponentHTML Query
  render state =
    HH.label_
      [ HH.p_ [ HH.text "What do you have to say?" ]
      , HH.input
          [ HP.value state
          , HE.onValueInput (HE.input HandleInput)
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX, console :: CONSOLE | m))
  eval (HandleInput value next) = do
    -- s <- H.get
    -- H.put $ s <> value
    _ <- H.put value
    -- foo <- H.liftAff $ AX.get "http://date.jsontest.com/"
    -- _ <- H.liftEff $ logShow foo
    pure next
  eval (GetValue reply) = do
    reply <$> H.get
