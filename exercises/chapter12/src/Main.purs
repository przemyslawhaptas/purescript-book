module Main where

import Prelude

import Control.Monad.Cont.Trans (runContT)
import Effect (Effect)
import Effect.Console (log, error)
import Control.Monad.Trans.Class (lift)
import Data.Either (either)
import Network.HTTP.Client (getCont)
import Types (Async)

main :: Effect Unit
main = async do
    response <- getCont "http://purescript.org"
    lift (either error log response)
  where
    async :: Async Unit -> Effect Unit
    async = flip runContT pure
