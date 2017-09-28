module RandomNumber where

import Prelude

import Control.Monad.Eff.Random (random)
import Control.Monad.Eff.Console (logShow)

getRandomNumber = do
  n <- random
  logShow n
  
