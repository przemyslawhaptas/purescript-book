module Example.Exercises where

import Prelude

import Data.Int (toNumber)
import Data.Foldable (for_)
import Data.Array (uncons, (..), zipWith)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D, lineTo, moveTo,
                        setFillStyle, setStrokeStyle, strokePath, fillPath, rect)
import Partial.Unsafe (unsafePartial)

type Point = { x :: Number, y :: Number }

renderPath :: Context2D -> Array Point -> Effect Unit
renderPath ctx points = case uncons points of
  Just { head, tail } -> strokePath ctx $ do
    _ <- moveTo ctx head.x head.y
    for_ points \{ x, y } -> lineTo ctx x y

  Nothing -> strokePath ctx $ pure unit

sampleSize :: Int
sampleSize = 100

scale :: Int
scale = 1000

plot :: Context2D -> (Number -> Number) -> Effect Unit
plot ctx f = renderPath ctx $ toPoints samples (f <$> samples) where
  scaleUp { x, y } = { x: x * (toNumber scale), y: y * (toNumber scale) }
  toPoints xs ys = scaleUp <$> zipWith (\x y -> { x, y }) xs ys
  normalize a = (toNumber a) / (toNumber sampleSize)
  samples = normalize <$> (1 .. sampleSize)

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  _ <- setFillStyle ctx "#0000FF"
  _ <- setStrokeStyle ctx "#5555AA"

  _ <- fillPath ctx $ rect ctx
    { x: 250.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }

  _ <- strokePath ctx $ rect ctx
    { x: 350.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }

  plot ctx (\x -> x * x)
