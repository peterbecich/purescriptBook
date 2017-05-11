module Chapter6.Typeclasses where

import Prelude

import Data.Monoid
import Data.Foldable
import Math

-- class Show a where
--   show :: forall a. a -> String

-- orphan instance
-- instance showBoolean :: Show Boolean where
--   show true = "true"
--   show false = "false"


stringTrue = show true
stringFalse = show false

newtype Complex = Complex
                  { real :: Number
                  , imaginary :: Number
                  }

instance showComplex :: Show Complex where
  show (Complex { real: r, imaginary: i }) =
    (show r) <> " <> " <> (show i) <> "i"

modulus :: Complex -> Number
modulus (Complex { real: r, imaginary: i }) = 
  abs $ sqrt (pow r 2.0 + pow i 2.0)

instance eqComplex :: Eq Complex where
  eq (Complex { real: r1, imaginary: i1 }) (Complex { real: r2, imaginary: i2 })
    | r1 == r2 && i1 == i2 = true
    | otherwise = false


instance ordComplex :: Ord Complex where
  compare complex1 complex2
    | (modulus complex1) > (modulus complex2) = LT
    | otherwise = GT

  
  -- compare complex1 complex2
  --   | (modulus complex1) > (modulus complex2) = LT
  --   | otherwise = GT
  --   where 
  --     modulus :: Complex -> Number
  --     modulus (Complex { real: r, imaginary: i }) = 
  --       abs $ sqrt (pow r 2 + pow i 2)





