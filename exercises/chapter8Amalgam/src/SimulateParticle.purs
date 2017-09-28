module SimulateParticle where

import Prelude
import Control.Monad.Eff (Eff, forE, runPure)
import Control.Monad.ST (ST, newSTRef, readSTRef, modifySTRef, runST)


simulate :: forall eff h. Number ->
            Number ->
            Int ->
            Eff (st :: ST h | eff) Number
simulate x0 v0 time = do
  ref <- newSTRef { x: x0, v: v0 }
  _ <- forE 0 (time * 1000) \_ -> do
    _ <- modifySTRef ref \o -> { v: o.v - 9.81 * 0.001, x: o.x + o.v * 0.001 }
    pure unit
  final <- readSTRef ref
  pure final.x

simulate' :: Number -> Number -> Int -> Number
simulate' x0 v0 time =
  runPure (runST (simulate x0 v0 time))
