module Test.Exercises where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Effect.Exception (throwException, error)
import Effect.Random (randomInt)
import Data.Maybe (Maybe)
import Data.Array (head, tail, sort, nubEq, foldM)
import Data.List (List(..))
import Data.Int (toNumber)
import Control.Monad.ST.Ref as STRef
import Control.Monad.ST as ST
import Math (sqrt)

type AppProps = {}

main :: Effect Unit
main = logShow "Started"

third :: forall a. Array a -> Maybe a
third xs = do
  tailA <- tail xs
  tailB <- tail tailA
  head tailB

sums :: Array Int -> Array Int
sums elements = sort $ nubEq $ foldM (\acc el -> [acc, el, acc + el]) 0 elements

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM f Nil = pure Nil
filterM f (Cons x xs) = do
  meetsCondition <- f x
  if meetsCondition
    then
      pure (Cons x) <*> (filterM f xs)
    else
      filterM f xs

unsafeDivide :: Int -> Int -> Effect Int
unsafeDivide _ 0 = throwException (error "You cannot divide by 0!")
unsafeDivide a b = pure (a / b)

inCircle :: Int -> Int -> Int -> Boolean
inCircle r x y = distance <= toNumber r where
  distance = sqrt $ toNumber $ ((r - x) * (r - x)) + ((r - y) * (r - y))

estimatePi :: Effect Number
estimatePi = do
  a <- randomInt 1000 10000
  let
    n = a * a
    r = a / 2
    countInCircle = ST.run do
      total <- STRef.new 0
      ST.for 0 n \i -> do
        let
          y = i / a
          x = i - y * a
        STRef.modify (if (inCircle r x y) then ((+) 1) else identity) total
      STRef.read total
  pure $ (toNumber (4 * countInCircle)) / (toNumber n)
