module Timeout where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Cont.Trans (ContT(..))
import Control.Parallel (parallel, sequential)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Types (Async)

type Milliseconds = Int

foreign import setTimeoutImpl :: Fn2 (Effect Unit)
                                     Milliseconds
                                     (Effect Unit)

setTimeout :: Milliseconds -> (Unit -> Effect Unit) -> Effect Unit
setTimeout ms k = runFn2 setTimeoutImpl (k unit) ms

setTimeoutCont :: Milliseconds -> Async Unit
setTimeoutCont ms = ContT $ setTimeout ms

timeout :: forall a. Milliseconds -> Async a -> Async (Maybe a)
timeout ms k = sequential $
  parallel (Just <$> k) <|> parallel ((\_ -> Nothing) <$> setTimeoutCont ms)

-- flip runContT logShow (timeout 2000 (setTimeoutCont 1000))
