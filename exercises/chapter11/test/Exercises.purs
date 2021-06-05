module Test.Exercises where

import Prelude

import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, execState)
import Control.Monad.State.Class (modify)
import Data.Foldable (traverse_, fold)
import Data.List.Lazy (replicate)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (log)

sumArray :: Array Number -> State Number Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

main :: Effect Unit
main = log $ render $ cat
  [ line "Here is some indented text:"
  , indent $ cat
      [ line "I am indented"
      , line "So am I"
      , indent $ line "I am even more indented"
      ]
  ]

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

type Level = Int
type Doc = Reader Level String

line :: String -> Doc
line str = do
  level <- ask
  pure $ (fold (replicate level " ")) <> str

indent :: Doc -> Doc
indent = local ((+) 1)

cat :: Array Doc -> Doc
cat docs = do
  strings <- sequence docs
  pure $ joinWith "\n" strings

render :: Doc -> String
render doc = runReader doc 0
