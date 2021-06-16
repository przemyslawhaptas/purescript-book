module Word where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, (..))
import Data.Maybe (fromMaybe)
import Data.Char (fromCharCode, toCharCode)
import Data.String.CodeUnits (fromCharArray)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (arrayOf, elements)

newtype Word = Word String

derive newtype instance showWord :: Show Word

instance arbWord :: Arbitrary Word where
  arbitrary = (Word <<< fromCharArray) <$> arrayOf (elements aToZ)

aToZ :: NonEmptyArray Char
aToZ = map (fromMaybe 'a' <<< fromCharCode) $ (toCharCode 'a') .. (toCharCode 'z')
