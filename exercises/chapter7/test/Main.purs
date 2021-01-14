module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.Maybe (Maybe(..))
import Control.Apply (lift2)

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
