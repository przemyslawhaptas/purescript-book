module Data.Action
  ( class Action
  , act
  , Multiply(..)
  , Self(..)
  ) where

import Prelude
import Data.Array (replicate)
import Data.Foldable (foldl)

newtype Multiply = Multiply Int

instance showMultiply :: Show Multiply where
  show (Multiply a) = "Multiply " <> show a

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply a) (Multiply b) = Multiply (a * b)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

class Monoid m <= Action m a where
  act :: m -> a -> a

instance repeatAction :: Action Multiply String where
  act (Multiply n) string = foldl (<>) "" (replicate n string)

instance actionArray:: Action m a => Action m (Array a) where
  act m elements = map (act m) elements

newtype Self m = Self m

instance showSelf :: Show a => Show (Self a) where
  show (Self a) = "Self " <> show a

instance actionSelf :: Monoid m => Action m (Self m) where
  act a (Self b) = Self (append a b)
