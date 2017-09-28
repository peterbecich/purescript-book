module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Network.HTTP.Affjax as AX

import ComponentC as CC

main :: forall e. Eff (HA.HalogenEffects (ajax :: AX.AJAX, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI CC.component unit body

  
