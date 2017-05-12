module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Random (random, RANDOM)
import Control.Monad.Eff.Console (logShow, CONSOLE)

-- The RANDOM effect indicates that an Eff action may access or modify the JavaScript global random number generator, i.e. Math.random().
-- The CONSOLE effect represents those computations which write to the console.
main :: forall eff. Eff (random :: RANDOM, console :: CONSOLE | eff) Unit
main = do
  n <- random
  logShow n
