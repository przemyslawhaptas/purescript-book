module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.Maybe (Maybe(..))
import Control.Apply (lift2)
import Data.Traversable (class Traversable, traverse, sequence, sequenceDefault)
import Data.Foldable (class Foldable, foldMap, foldrDefault, foldlDefault)

import Data.AddressBook (examplePerson)
import Data.AddressBook.Validation (validatePerson)

main :: Effect Unit
main = logShow (validatePerson examplePerson)

maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd = lift2 add

infixl 6 maybeAdd as +?

maybeSub :: Maybe Int -> Maybe Int -> Maybe Int
maybeSub = lift2 sub

infixl 6 maybeSub as -?

maybeMul :: Maybe Int -> Maybe Int -> Maybe Int
maybeMul = lift2 mul

infixl 6 maybeMul as *?

maybeDiv :: Maybe Int -> Maybe Int -> Maybe Int
maybeDiv = lift2 div

infixl 6 maybeDiv as /?

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just a) = Just <$> a

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance functorTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch left val right) = Branch (map f left) (f val) (map f right)

instance foldableTree :: Foldable Tree where
  foldMap f Leaf = mempty
  foldMap f (Branch left val right) = (foldMap f left) <> f val <> (foldMap f right)
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch left val right) = Branch <$> (traverse f left) <*> (f val) <*> (traverse f right) -- in-order
  sequence = sequenceDefault

mySequence :: forall t a m. Traversable t => Applicative m => t (m a) -> m (t a)
mySequence = traverse identity

myTraverse :: forall t a b m. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
myTraverse f t = sequence (map f t)
