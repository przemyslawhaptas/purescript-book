module Example.Random where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random (random)
import Graphics.Canvas (Context2D, arc, fillPath, getCanvasElementById, getContext2D, setFillStyle, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Utils.Canvas (randomColor, rotateAround)
import Utils.Web (attachEventListener, getClientPosition)

import Effect.Console (logShow)

strokeFillPath :: forall a. Context2D -> Effect a -> Effect a
strokeFillPath ctx path = do
  _ <- fillPath ctx path
  strokePath ctx path

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  attachEventListener "#canvas" "click" $ \e -> do
    renderRandomCircle ctx
    let { x, y } = getClientPosition e
    logShow x
    logShow y
    rotateAround ctx x y

renderRandomCircle :: Context2D -> Effect Unit
renderRandomCircle ctx = do
  x <- random
  y <- random
  r <- random
  fillColor <- randomColor
  strokeColor <- randomColor

  _ <- setFillStyle ctx fillColor
  _ <- setStrokeStyle ctx strokeColor

  let path = arc ctx
       { x      : x * 600.0
       , y      : y * 600.0
       , radius : r * 100.0
       , start  : 0.0
       , end    : Math.pi * 2.0
       }

  strokeFillPath ctx path
