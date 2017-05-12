module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Random (random, RANDOM)
import Control.Monad.Eff.Console (logShow, CONSOLE)
import Control.Monad.Eff.Exception
import Control.Monad.ST

import Chapter8.Effects

-- The RANDOM effect indicates that an Eff action may access or modify the JavaScript global random number generator, i.e. Math.random().
-- The CONSOLE effect represents those computations which write to the console.
main :: forall eff h.
        Eff (st :: ST h, random :: RANDOM, console :: CONSOLE, exception :: EXCEPTION | eff) Unit
main = do
  pi <- estimatePi 10000
  logShow pi


-- main :: forall eff.
--         Eff (random :: RANDOM, console :: CONSOLE, exception :: EXCEPTION | eff) Unit
-- main = do
--   quotient <- safeDivide 5.0 0.0
--   logShow quotient


-- main = do
--   n <- random
--   logShow n

