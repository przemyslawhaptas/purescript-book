module Test.Exercises where

import Prelude

import Control.Monad.State (State, execState)
import Control.Monad.State.Class (modify)
import Data.Foldable (traverse_)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Console (logShow)
import Data.Maybe (Maybe(..))

sumArray :: Array Number -> State Number Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

main :: Effect Unit
main = logShow "Exercises"

testParens :: String -> Boolean
testParens s = isBalanced $ execState
  (traverse_ (\c -> modify (countParen c)) (toCharArray s))
  (Just 0)
    where
    isBalanced (Just 0) = true
    isBalanced _ = false
    countParen _ Nothing = Nothing
    countParen char (Just count) = if newCount < 0 then Nothing else (Just newCount)
      where
      newCount = if char == '(' then count + 1 else count - 1
