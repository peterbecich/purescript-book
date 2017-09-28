module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception
import Control.Monad

import React.DOM as D
import React.DOM.Props as P

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)

import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReadWrite, ReactState, Event, ReactThis, ReactElement, createFactory, readState, spec, createClass, writeState)
import ReactDOM (render)


import CountThrows
import SimulateParticle


newtype Counter = Counter { x :: Int, y :: Int }

initialState :: Counter
initialState = Counter { x: 0, y: 0 }


main :: forall e. Eff (
  console :: CONSOLE, exception :: EXCEPTION, dom :: DOM | e
) Unit
main = do
  -- _ <- log $ show $ countThrows 4
  _ <- logShow $ countThrows 4
  -- _ <- throwException $ error "bogus error"
  _ <- log "Hello sailor!"
  win <- window
  doc <- document win
  
  logShow $ simulate' 10.0 100.0 10
