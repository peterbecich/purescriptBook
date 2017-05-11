module Chapter4.Recursion where

import Prelude

import Data.Array (null)
import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)
import Data.Array
import Control.MonadZero (guard)
import Data.Int (pow)
import Data.Foldable
-- import Data.List (fmap)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- > :t unsafePartial 
-- forall a. (Partial => a) -> a

length :: forall a. Array a -> Int
length arr =
  if null arr
     then 0
     else 1 + length (unsafePartial tail arr)

numStrings = show <$> [1, 2, 3, 4, 5]
numStrings' = map show [1, 2, 3, 4, 5]

positive :: Array Int -> Array Int
positive arr = filter (\i -> i>0) arr

infix 8 filter as <$?>

positive' :: Array Int -> Array Int
positive' arr = (\i -> i>0) <$?> arr

pairs n = concatMap (\i -> 1 .. n) (1 .. n)
threePair = pairs 3

-- factors :: Int -> Array (Array Int)
-- factors n = filter (\xs -> product xs == n) $ do
--   i <- 1 .. n
--   j <- i .. n
--   pure [i, j]

-- > :t guard
-- forall m. MonadZero m => Boolean -> m Unit
factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

pythagoreanTriples :: Int -> Array (Array Int)
pythagoreanTriples n = do
  i <- 1 .. n
  j <- i .. n
  k <- 1 .. n
  guard $ (pow i 2) + (pow j 2) <= (pow k 2)
  pure [i, j, k]


-- 4.12

