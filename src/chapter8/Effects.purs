module Chapter8.Effects where

import Prelude

import Data.Maybe
import Data.List
import Data.Int (toNumber)

import Math

import Control.Monad.Eff
import Control.Monad.Eff.Random (random, RANDOM)
-- import Control.Monad.Eff.Console (logShow, CONSOLE)

import Control.Monad.Eff.Exception

-- import Control.Monad.Eff (Eff, forE)
import Control.Monad.ST (ST, newSTRef, readSTRef, modifySTRef, runST)

simulate :: forall eff h. Number -> Number -> Int -> Eff (st :: ST h | eff) Number
simulate x0 v0 time = do
  ref <- newSTRef { x: x0, v: v0 }
  _ <- forE 0 (time * 1000) \_ -> do
    _ <- modifySTRef ref \o -> { v: o.v - 9.81 * 0.001, x: o.x + o.v * 0.001 }
    pure unit
  final <- readSTRef ref
  pure final.x

simulate' :: Number -> Number -> Int -> Number
simulate' x0 v0 time = runPure (runST (simulate x0 v0 time))

safeDivideMaybe :: Int -> Int -> Maybe Int
safeDivideMaybe _ 0 = Nothing
safeDivideMaybe a b = Just (a / b)

justFive' = foldM safeDivideMaybe 100 (fromFoldable [5, 2, 2])
notFive' = foldM safeDivideMaybe 100 (fromFoldable [2, 0, 4])


-- rewrite `safeDivide` to throw an exception
-- using `throwException` if the denominator is zero

-- throwException :: forall a eff
--                 . Error
--                -> Eff (err :: EXCEPTION | eff) a

-- catchException :: forall a eff
--                 . (Error -> Eff eff a)
--                -> Eff (err :: EXCEPTION | eff) a
--                -> Eff eff a


safeDivide :: forall eff.
              Number ->
              Number ->
              Eff (exception :: EXCEPTION | eff) Number
safeDivide numer 0.0 = throwException $ error "denominator is zero"
safeDivide numer denom = pure (numer / denom)

-- :t runPure 
-- forall a. Eff () a -> a

-- obviously not mathematically correct
safeDivideHandled numer denom = catchException (\_ -> pure 0.0) $ safeDivide numer denom

safeDivide' numer denom = runPure $ safeDivideHandled numer denom


-- (Difficult) The following is a simple way to estimate pi: randomly choose a large number N of points in the unit square, and count the number n which lie in the inscribed circle. An estimate for pi is 4n/N. Use the RANDOM and ST effects with the forE function to write a function which estimates pi in this way.

-- :t forE
-- forall e. Int -> Int -> (Int -> Eff e Unit) -> Eff e Unit

-- :t modifySTRef 
-- forall a h r. STRef h a -> (a -> a) -> Eff ( st :: ST h | r ) a           

norm :: Number -> Number -> Number
norm x y = sqrt (pow x 2.0 + pow y 2.0)

estimatePi :: forall eff h.
              Int ->
              Eff (st :: ST h, random :: RANDOM | eff) Number
estimatePi iterations = do
  ref <- newSTRef 0
  _ <- forE 0 iterations (\_ -> do
                             x <- random
                             y <- random
                             _ <- modifySTRef ref (\countInCircle -> let
                                                      nm = norm x y
                                                      in if (nm <= 1.0) then (countInCircle + 1) else countInCircle
                                                  )
                             pure unit
                         )
  countInCircle <- readSTRef ref
  let iterationsNum = toNumber iterations
  pure $ (4.0*(toNumber countInCircle))/iterationsNum
