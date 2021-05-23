module Example.Refs where

import Prelude

import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (read, modify_, new)
import Graphics.Canvas (Context2D, getContext2D, getCanvasElementById, rect, fillPath, translate, scale, rotate, withContext, setFillStyle)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

render :: Int -> Context2D -> Effect Unit
render count ctx = void do
  _ <- setFillStyle ctx "#FFFFFF"

  _ <- fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: 600.0
    , height: 600.0
    }

  _ <- setFillStyle ctx "#00FF00"

  withContext ctx do
    let scaleX = Math.sin (toNumber count * Math.pi / 4.0) + 1.5
    let scaleY = Math.sin (toNumber count * Math.pi / 6.0) + 1.5

    _ <- translate ctx { translateX: 300.0, translateY:  300.0 }
    _ <- rotate ctx (toNumber count * Math.pi / 18.0)
    _ <- scale ctx { scaleX: scaleX, scaleY: scaleY }
    _ <- translate ctx { translateX: -100.0, translateY: -100.0 }

    fillPath ctx $ rect ctx
      { x: 0.0
      , y: 0.0
      , width: 200.0
      , height: 200.0
      }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  clickCount <- new 0

  render 0 ctx

  doc <- window >>= document
  node <- querySelector (QuerySelector "#canvas") (toParentNode doc)
  onClick <- eventListener $ \e -> do
        log "Mouse clicked!"
        modify_ (\count -> count + 1) clickCount
        count <- read clickCount
        render count ctx
  for_ (toEventTarget <$> node) $ addEventListener (EventType "click") onClick false
