module Control.Monad.Eff.DOM where

import Prelude

import Effect (Effect)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))

foreign import data Node :: Type

foreign import querySelectorImpl
  :: forall eff r
   . Fn3 r
         (Node -> r)
         String
         (Effect r)

querySelector
  :: forall eff
   . String
  -> Effect (Maybe Node)
querySelector s = runFn3 querySelectorImpl Nothing Just s

foreign import addEventListener
  :: forall eff
   . String
  -> Effect Unit
  -> Node
  -> Effect Unit
