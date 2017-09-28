module AffjaxTest where

import Prelude
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (launchAff)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax (affjax, defaultRequest)

main = launchAff $ do
  res <- affjax $ defaultRequest { url = "/api", method = Left GET }
  liftEff $ log $ "GET /api response: " <> res.response

  
