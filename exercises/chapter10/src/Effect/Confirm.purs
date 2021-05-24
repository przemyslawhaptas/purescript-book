module Effect.Confirm where

import Prelude

import Effect (Effect)

foreign import confirm :: String -> Effect Unit
