module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.Hashable (hash, hashEqual, class Hashable)
import Data.Foldable (class Foldable, foldr, foldl, foldMap, maximum)
import Data.Stream (class Stream, uncons, foldStream)
import Data.Maybe (fromJust)
import Data.Array (nubByEq, length)

main :: Effect Unit
main = do
  logShow (hash 123)
  logShow (hash true)
  logShow (hash [1, 2, 3])
  logShow (hash "testing")
  logShow (hash 'a')
  logShow ("foo" `hashEqual` "foo")
  logShow ("foo" `hashEqual` "bar")

data Point = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point { x, y }) =
    "(" <> show x <> ", " <> show y <> ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Picture

type Picture = Array Shape

instance showShape :: Show Shape where
  show (Circle c r) =
    "Circle [center: " <> show c <> ", radius: " <> show r <> "]"
  show (Rectangle c w h) =
    "Rectangle [center: " <> show c <> ", width: " <> show w <> ", height: " <> show h <> "]"
  show (Line start end) =
    "Line [start: " <> show start <> ", end: " <> show end <> "]"
  show (Text loc text) =
    "Text [location: " <> show loc <> ", text: " <> show text <> "]"
  show (Clipped picture) =
    foldl (<>) "" (map show picture)

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) =
    show real <> " + " <> show imaginary <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex complex1) (Complex complex2) =
    (complex1.real == complex2.real) && (complex1.imaginary == complex2.imaginary)

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty head1 tail1) (NonEmpty head2 tail2) =
    (head1 == head2) && (tail1 == tail2)

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty head1 tail1) (NonEmpty head2 tail2) =
    NonEmpty head1 (tail1 <> [head2] <> tail2)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty head tail) =
    NonEmpty (f head) (map f tail)

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq (Finite el1) (Finite el2) = el1 == el2
  eq Infinite Infinite = true
  eq _ _ = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Finite el1) (Finite el2) = compare el1 el2
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr foo acc (NonEmpty head tail) = foo head (foldr foo acc tail)
  foldl foo acc (NonEmpty head tail) = foldl foo (foo acc head) tail
  foldMap foo (NonEmpty head tail) = foo head <> foldMap foo tail

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr foo acc (OneMore head tail) = foo head (foldr foo acc tail)
  foldl foo acc (OneMore head tail) = foldl foo (foo acc head) tail
  foldMap foo (OneMore head tail) = foo head <> foldMap foo tail

partialMaximum :: Partial => Array Int -> Int
partialMaximum = fromJust <<< maximum

hasDuplicates :: forall a. Hashable a => Array a -> Boolean
hasDuplicates elements =
  length elements /= length (nubByEq quickCompare elements)
  where
    quickCompare a b = hashEqual a b && a == b

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashHour :: Hashable Hour where
  hash (Hour a) = hash (mod a 12)
