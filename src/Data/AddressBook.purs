module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Control.Monad.State
import Control.Applicative
import Control.Apply
import Data.List
import Data.Array as Arr
import Data.Array ((..))
import Data.Maybe
import Data.Set as S
import Data.Identity
import Data.Ord
import Data.Ordering

-- http://brianhamrick.com/blog/records-haskell-purescript

newtype Entry = Entry
  { firstName :: String
  , lastName :: String
  , address :: Address
  }


derive instance ordEntry :: Ord Entry
derive instance eqEntry :: Eq Entry

newtype Address = Address
  { street :: String
  , city :: String
  , state :: String
  }

fooAddr :: Address
fooAddr = Address { street: "foo", city: "Los Angeles", state: "California" }
barAddr :: Address
barAddr = Address { street: "foo", city: "San Francisco", state: "California" }
fooEntry :: Entry
fooEntry = Entry { firstName: "Peter", lastName: "Becich", address: fooAddr }

address :: String -> String -> String -> Address
address str cty stt = Address { street: str, city: cty, state: stt }

livesInLA :: Address -> Boolean
livesInLA (Address { city: "Los Angeles" }) = true
livesInLA _ = false

-- http://stackoverflow.com/questions/40508767/convert-an-array-to-a-list-in-purescript
-- arrayToList :: forall a. Array a -> List a
-- arrayToList arr = fromFoldable ( Arr.toUnfoldable arr )

duplicatedBook :: AddressBook
duplicatedBook = foldr insertEntry emptyBook [fooEntry, fooEntry, fooEntry, fooEntry]

deduplicatedBook :: AddressBook
deduplicatedBook = removeDuplicates duplicatedBook

derive instance ordAddress :: Ord Address
derive instance eqAddress :: Eq Address

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry (Entry entry) = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

instance shwEnt :: Show Entry where
  show ent = showEntry ent

showAddress :: Address -> String
showAddress (Address addr) = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

instance shwAddr :: Show Address where
  show addr = showAddress addr

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry ent addrbook = Cons ent addrbook

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry first last addrBook = (head <<< filter filterPredicate) addrBook
  where
    filterPredicate :: Entry -> Boolean
    filterPredicate (Entry entry) = entry.firstName == first && entry.lastName == last

-- requires Ord instance for Address and Entry
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates addrBook = let
  distinctFilter = filterM distinctPredicate
  stateDistinct = distinctFilter addrBook
  in evalState stateDistinct S.empty

-- -- https://github.com/purescript/purescript/blob/master/examples/passing/Rank2Data.purs
runId :: forall a. Identity a -> a
runId (Identity x) = x

distinctPredicate :: forall a. (Ord a) => a -> State (S.Set a) Boolean
distinctPredicate x = do
  set <- get
  let exsts = S.member x set
  _ <- put $ S.insert x set
  pure $ not exsts


somePlace :: Maybe Address
somePlace = lift3 address (Just "123 Fake St.") (Just "The City") (Just "California")

somePlace' :: Maybe Address
somePlace' = address <$> (Just "123 Fake St.") <*> (Just "The City") <*> (Just "California")

countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 6
  if x + y == n
    then pure [x, y]
    else empty

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)

-- 100 / 5 / 2 / 2 == 5
five = foldM safeDivide 100 (fromFoldable [5, 2, 2])

-- 100 / 2 / 0 / 4
notFive = foldM safeDivide 100 (fromFoldable [2, 0, 4])


