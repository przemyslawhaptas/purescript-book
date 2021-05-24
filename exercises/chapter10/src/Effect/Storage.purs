module Effect.Storage where

import Prelude

import Effect (Effect)
import Foreign (Foreign)

foreign import setItem :: forall eff. String -> String -> Effect Unit

foreign import getItem :: forall eff. String -> Effect Foreign
