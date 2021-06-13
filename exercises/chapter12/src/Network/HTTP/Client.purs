module Network.HTTP.Client where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Parallel (parallel, sequential)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Effect.Console (logShow)
import Files (writeFileContEx, FilePath)
import Types (Async)

type URI = String

foreign import getImpl :: Fn3 URI
                              (String -> Effect Unit)
                              (String -> Effect Unit)
                              (Effect Unit)

getCont :: URI -> Async (Either String String)
getCont req = ContT $ \k -> runFn3 getImpl req (k <<< Right) (k <<< Left)

getContEx :: URI -> ExceptT String Async String
getContEx req = ExceptT $ getCont req

downloadContEx :: URI -> FilePath -> ExceptT String Async Unit
downloadContEx req dest = do
  response <- getContEx req
  writeFileContEx dest response

download :: URI -> FilePath -> Effect Unit
download req dest = runContT (runExceptT $ downloadContEx req dest) mempty

getTwo :: URI -> URI -> Effect Unit
getTwo req1 req2 = flip runContT logShow do
  sequential $
    lift2 append
      <$> parallel (getCont req1)
      <*> parallel (getCont req2)
