module Effect.Alert where

import Prelude

import Effect (Effect)

foreign import alert :: forall eff. String -> Effect Unit
