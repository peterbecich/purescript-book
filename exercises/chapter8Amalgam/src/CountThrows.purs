module CountThrows where

import Prelude

import Control.Plus (empty)
import Data.Array ((..))

countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 7
  if x + y == n
    then pure [x, y]
    else empty


