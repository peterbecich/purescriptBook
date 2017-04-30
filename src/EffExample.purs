module EffExample where

import Prelude

import Control.Monad.Eff.Random (random)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff

main = do
  n <- random
  logShow n
