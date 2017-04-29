module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Control.Monad.State
import Data.List
import Data.Array as Arr
import Data.Maybe (Maybe)
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
fooAddr = Address { street: "foo", city: "bar", state: "California" }
fooEntry :: Entry
fooEntry = Entry { firstName: "Peter", lastName: "Becich", address: fooAddr }

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


