module Utils.Canvas where

import Prelude

import Data.Int (hexadecimal, pow, toNumber, floor, toStringAs)
import Effect (Effect)
import Effect.Random (random)
import Graphics.Canvas (Context2D, rotate, translate)
import Math as Math

hexLimit :: Int
hexLimit = pow 16 6

randomColor :: Effect String
randomColor = do
  x <- random
  let randomHexInt = floor $ x * toNumber hexLimit
  pure $ "#" <> toStringAs hexadecimal randomHexInt

rotateAround :: Context2D -> Int -> Int -> Effect Unit
rotateAround ctx x y = do
  rotation <- ((*) Math.pi) <$> random
  _ <- translate ctx { translateX: toNumber x, translateY: toNumber y }
  _ <- rotate ctx rotation
  translate ctx { translateX: toNumber (-x), translateY: toNumber (-y) }
