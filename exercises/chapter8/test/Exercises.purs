module Test.Exercises where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Maybe (Maybe)
import Data.Array ((:), head, tail, sort, nubEq, foldM)
import Data.List (List(..))
import Control.MonadPlus (guard)

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
