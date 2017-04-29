module Diagonal where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Math (sqrt)

diagonal w h = sqrt (w * w + h * h)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = logShow (diagonal 3.0 4.0)

