module Network.HTTP.Client where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Files (writeFileContEx, FilePath)
import Types (Async)

type URI = String

foreign import getImpl :: Fn3 URI
                              (String -> Effect Unit)
                              (String -> Effect Unit)
                              (Effect Unit)

get :: URI -> Async (Either String String)
get req = ContT $ \k -> runFn3 getImpl req (k <<< Right) (k <<< Left)

getEx :: URI -> ExceptT String Async String
getEx req = ExceptT $ get req

downloadEx :: URI -> FilePath -> ExceptT String Async Unit
downloadEx req dest = do
  response <- getEx req
  writeFileContEx dest response

download :: URI -> FilePath -> Effect Unit
download req dest = runContT (runExceptT $ downloadEx req dest) mempty
