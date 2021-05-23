module Utils.Web where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)
import Web.UIEvent.UIEvent (fromEvent)
import Web.UIEvent.MouseEvent (clientX, clientY, fromUIEvent)
import Data.Maybe (Maybe(..))

attachEventListener :: forall a. String -> String -> (Event -> Effect a) -> Effect Unit
attachEventListener selector event listener = do
    doc <- window >>= document
    node <- querySelector (QuerySelector selector) (toParentNode doc)
    listener_ <- eventListener listener
    for_ (toEventTarget <$> node) $ addEventListener (EventType event) listener_ false

getClientPosition :: Event -> { x :: Int, y :: Int }
getClientPosition e =
  case (fromEvent e >>= fromUIEvent) of
    Just mouseEvent -> { x: clientX mouseEvent, y: clientY mouseEvent }
    Nothing -> { x: 0, y: 0 }
