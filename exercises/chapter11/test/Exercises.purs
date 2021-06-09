module Test.Exercises where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Except (ExceptT, runExcept, runExceptT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Reader.Class (ask, local)
import Control.Monad.State (State, execState, StateT, runStateT)
import Control.Monad.State.Class (modify, get, put)
import Control.Monad.Writer (Writer, execWriter, runWriter, runWriterT, WriterT)
import Control.Monad.Writer.Class (tell)
import Data.Foldable (traverse_, fold)
import Data.Identity (Identity)
import Data.List (List, many)
import Data.List.Lazy (replicate)
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Tuple (Tuple)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (joinWith, drop, take)
import Data.String.CodeUnits (stripPrefix, toCharArray)
import Data.String.Pattern (Pattern(..))
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
  , line "#sumArrayW"
  , indent $ line "in: [1.0, 2.0, 3.0, 4.0, 5.0]"
  , indent $ line $ "out " <> show (unwrap $ execWriter $ sumArrayW [1.0, 2.0, 3.0, 4.0, 5.0])
  , line "#safeDivide"
  , indent $ line "in: 4, 0"
  , indent $ line $ "out " <> show (runExcept $ safeDivide 4 0)
  , indent $ line "in: 4, 2"
  , indent $ line $ "out " <> show (runExcept $ safeDivide 4 2)
  , line "#string"
  , indent $ line "in: prefix: abc, string: abcdef"
  , indent $ line $ "out " <> show (runParser (string "abc") "abcdef")
  , indent $ line "in: prefix: def, string: abcdef"
  , indent $ line $ "out " <> show (runParser (string "def") "abcdef")
  , line "#asThenBs"
  , indent $ line $ "out " <> show (runParser asThenBs "aaaaabaaacabcdef")
  , line "#asOrBs"
  , indent $ line $ "out " <> show (runParser asOrBs "aaaaabaaacabcdef")
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

safeDivide :: Int -> Int -> ExceptT String Identity Int
safeDivide a b = do
  if b == 0
    then throwError "Not possible to divide by 0!"
    else pure $ a / b

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

runParser :: forall a. Parser a -> String -> Either Errors (Tuple (Tuple a String) Log)
runParser p s = runExcept $ runWriterT $ runStateT p s

split :: Parser String
split = do
  s <- get
  tell ["The state is " <> show s]
  case s of
    "" -> throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

string :: String -> Parser String
string prefix = do
  s <- get
  tell ["The state is " <> show s]
  case (stripPrefix (Pattern prefix) s) of
    Nothing -> throwError [prefix <> " is not a prefix of " <> s <> "!"]
    Just suffix -> do
      put suffix
      pure prefix

asThenBs :: Parser (List String)
asThenBs = do
  as <- many (string "a")
  bs <- many (string "b")
  pure $ as <> bs

asOrBs :: Parser (List String)
asOrBs = many (string "a" <|> string "b")
