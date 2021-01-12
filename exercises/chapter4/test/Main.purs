module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.Path (root)
import Data.Foldable (for_, foldl)
import FileOperations (allFiles)

import Data.Array (null, filter, (..), length, (:))
import Data.Array.Partial (head, tail)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)

main :: Effect Unit
main = for_ (allFiles root) logShow

isEven :: Int -> Boolean
isEven 0 = true
isEven a = isOdd (a - 1)

isOdd :: Int -> Boolean
isOdd 0 = false
isOdd a = isEven (a - 1)

isEven2 :: Int -> Boolean
isEven2 0 = true
isEven2 1 = false
isEven2 a = isEven2 (a - 2)

countEven :: Array Int -> Int
countEven = countEven' 0
  where
    countEven' acc arr =
      if null arr
        then acc
        else countEven' updatedCount arrTail
      where
      updatedCount = if (isEven2 $ unsafePartial head arr)
        then acc + 1
        else acc
      arrTail = unsafePartial tail arr

squares :: Array Int -> Array Int
squares = map (\a -> a * a)

positives :: Array Int -> Array Int
positives = (<$?>) $ (\a -> a > 0)

infixr 8 filter as <$?>

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = length (factors n) == 1

cartesianProduct :: forall a b. Array a -> Array b -> Array (Tuple a b)
cartesianProduct arrA arrB = do
  a <- arrA
  b <- arrB
  pure $ Tuple a b

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- 1 .. n
  c <- 1 .. n
  guard $ a * a + b * b == c * c
  pure [a, b, c]

-- How to get rid of the duplicates?
factorizations :: Int -> Array (Array Int)
factorizations n = factorizations' n []
  where
    factorizations' n acc = do
      i <- 1 .. (div n 2)
      guard $ (mod n i) == 0
      if i == 1
        then pure (acc <> [n])
        else factorizations' (div n i) (acc <> [i])

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

count :: forall a. (a -> Boolean) -> Array a -> Int
count = count' 0
  where
    count' acc _ [] = acc
    count' acc p xs =
      if p (unsafePartial head xs)
        then count' (acc + 1) p (unsafePartial tail xs)
        else count' acc p (unsafePartial tail xs)


reverseFoldl :: Array Int -> Array Int
reverseFoldl = foldl (\as a -> a : as) []
