module Timeout where

import Prelude

import Control.Monad.Cont.Trans (ContT(..))
import Data.Function.Uncurried (Fn2, runFn2)
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
