module Chapter5.PatternMatching where

import Prelude
import Data.AddressBook
import Global as Global
import Math as Math
import Data.Array.Partial (tail)
import Data.Foldable (fold, foldl)
import Partial.Unsafe (unsafePartial)

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m
             then gcd (n - m) m
             else gcd n (m - n)



fromString :: String -> Boolean
fromString "true" = true
fromString _ = false


gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m
  | n > m = gcd' (n - m) m
  | otherwise = gcd' n (m - n)


isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

takeFive :: Array Int -> Int
takeFive [0, 1, a, b, _] = a * b
takeFive _ = 0

peter = Entry { firstName: "Peter", lastName: "Becich", address: fooAddr }

peter' = { firstName: "Peter", lastName: "Becich", city: "Long Beach" }

peter'' = { lastName: "Becich", city: "Long Beach" }


showName :: Entry -> String
showName (Entry {firstName: first, lastName: last}) =
  first <> " " <> last

-- for any object that contains firstName, lastName fields
-- showAnyName :: { firstName :: String, lastName :: String} -> String
showAnyName { firstName: first, lastName: last } =
  first <> " " <> last

sum :: Array Int -> Int
sum [] = 0
sum arr = foldl (+) 0 arr

lzs :: Array Int -> Array Int
lzs [] = []
lzs xs = case sum xs of
  0 -> xs
  _ -> lzs (unsafePartial tail xs)

  


