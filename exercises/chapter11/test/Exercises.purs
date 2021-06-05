module Test.Exercises where

import Prelude

import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, execState)
import Control.Monad.State.Class (modify)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Foldable (traverse_, fold)
import Data.List.Lazy (replicate)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (log)
import Data.Newtype (unwrap)

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
  , line "#sumArrayW"
  , indent $ line "in: [1.0, 2.0, 3.0, 4.0, 5.0]"
  , indent $ line $ "out " <> show (unwrap $ execWriter $ sumArrayW [1.0, 2.0, 3.0, 4.0, 5.0])
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

sumArrayW :: Array Number -> Writer (Additive Number) Unit
sumArrayW = traverse_ \n -> tell (Additive n)

collatz :: Int -> Int
collatz = collatz_ 0 where
  collatz_ count 1 = count
  collatz_ count n = collatz_ (count + 1) (if n `mod` 2 == 0 then (n / 2) else (3 * n + 1))

collatzW :: Int -> Writer (Array String) Int
collatzW = collatz_ 0 where
  collatz_ count 1 = pure count
  collatz_ count n = do
    tell ["collatz_ " <> show count <> " " <> show n]
    collatz_ (count + 1) (if n `mod` 2 == 0 then (n / 2) else (3 * n + 1))
