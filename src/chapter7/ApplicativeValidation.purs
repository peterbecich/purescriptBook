module Chapter7.ApplicativeValidation where

import Prelude

import Data.AddressBook

import Data.Maybe
import Control.Apply

-- orphan
-- instance functorMaybe :: Functor Maybe where
--   map f (Just a) = Just $ f a
--   map _ _ = Nothing

-- orphan
-- instance applyMaybe :: Apply Maybe where
--   apply (Just f) (Just a) = Just $ f a
--   apply _ _ = Nothing

lift :: forall f a b c . Apply f
         => (a -> b)
         -> f a
         -> f b
lift f fx = f <$> fx

lift2 :: forall f a b c d. Apply f
         => (a -> b -> c)
         -> f a
         -> f b
         -> f c
lift2 f fx fy = f <$> fx <*> fy

lift3 :: forall f a b c d e. Apply f
         => (a -> b -> c -> d)
         -> f a
         -> f b
         -> f c
         -> f d
lift3 f fx fy fz = f <$> fx <*> fy <*> fz

somePlace = lift3 address (Just "123 Fake St.") (Just "The City") (Just "California")
