module Data.DOM.Name
  ( Attribute
  , Name
  , Content
  , ContentF
  , AttributeKey
  , class IsValue
  , toValue
  , Href(..)

  , a
  , p
  , img

  , href
  , _class
  , src
  , width
  , height
  , name

  , attribute, (:=)
  , text
  , newName
  , isMobile

  , render
  ) where

import Prelude

import Control.Monad.Free (Free, runFreeM, liftF)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (put, get)
import Control.Monad.Writer.Trans (WriterT, execWriterT, tell)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Content Unit)
  }

newtype Name = Name String

data ContentF a
  = TextContent String a
  | ElementContent Element a
  | NewName (Name -> a)
  | IsMobile (Boolean -> a)

instance functorContentF :: Functor ContentF where
  map f (TextContent s x) = TextContent s (f x)
  map f (ElementContent e x) = ElementContent e (f x)
  map f (NewName k) = NewName (f <<< k)
  map f (IsMobile k) = IsMobile (f <<< k)

-- type Content a = Free ContentF a

newtype Content a = Content (Free ContentF a)
derive newtype instance monadContent :: Monad Content
derive newtype instance functorContent :: Functor Content
derive newtype instance applyContent :: Apply Content
derive newtype instance applicativeContent :: Applicative Content
derive newtype instance bindContent :: Bind Content

newtype Attribute = Attribute
  { key          :: String
  , value        :: String
  }

newtype AttributeKey :: forall k. k -> Type
newtype AttributeKey a = AttributeKey String

element :: String -> Array Attribute -> Maybe (Content Unit) -> Element
element name_ attribs content = Element { name: name_, attribs, content }

text :: String -> Content Unit
text s = Content $ liftF $ TextContent s unit

elem :: Element -> Content Unit
elem e = Content $ liftF $ ElementContent e unit

newName :: Content Name
newName = Content $ liftF $ NewName identity

isMobile :: Content Boolean
isMobile = Content $ liftF $ IsMobile identity

class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = identity

instance intIsValue :: IsValue Int where
  toValue = show

instance nameIsValue :: IsValue Name where
  toValue (Name n) = n

attribute :: forall a. IsValue a => AttributeKey a -> a -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: toValue value
  }

infix 4 attribute as :=

a :: Array Attribute -> Content Unit -> Content Unit
a attribs content = elem $ element "a" attribs (Just content)

p :: Array Attribute -> Content Unit -> Content Unit
p attribs content = elem $ element "p" attribs (Just content)

img :: Array Attribute -> Content Unit
img attribs = elem $ element "img" attribs Nothing

data Href
  = URLHref String
  | AnchorHref Name

instance hrefIsValue :: IsValue Href where
  toValue (URLHref url) = url
  toValue (AnchorHref (Name nm)) = "#" <> nm

href :: AttributeKey Href
href = AttributeKey "href"

name :: AttributeKey Name
name = AttributeKey "name"

_class :: AttributeKey String
_class = AttributeKey "class"

src :: AttributeKey String
src = AttributeKey "src"

width :: AttributeKey Int
width = AttributeKey "width"

height :: AttributeKey Int
height = AttributeKey "height"

type Interp = ReaderT Boolean (WriterT String (State Int))

render :: Boolean -> Content Unit -> String
render mobile initE = evalState (execWriterT (runReaderT (renderInitElement initE) mobile)) 0
  where
    renderInitElement :: Content Unit -> Interp Unit
    renderInitElement (Content elementContent) = do
      runFreeM renderContentItem elementContent

    renderContentItem :: forall a. ContentF (Free ContentF a) -> Interp (Free ContentF a)
    renderContentItem (TextContent s rest) = do
      tell s
      pure rest
    renderContentItem (ElementContent e' rest) = do
      renderElement e'
      pure rest
    renderContentItem (NewName k) = do
      n <- get
      let fresh = Name $ "name" <> show n
      put $ n + 1
      pure (k fresh)
    renderContentItem (IsMobile k) = do
      m <- ask
      pure (k m)

    renderElement :: Element -> Interp Unit
    renderElement (Element e) = do
      tell "<"
      tell e.name
      for_ e.attribs $ \x -> do
        tell " "
        renderAttribute x
      renderContent e.content
      where
        renderAttribute :: Attribute -> Interp Unit
        renderAttribute (Attribute x) = do
          tell x.key
          tell "=\""
          tell x.value
          tell "\""

        renderContent :: Maybe (Content Unit) -> Interp Unit
        renderContent Nothing = tell " />"
        renderContent (Just (Content content)) = do
          tell ">"
          runFreeM renderContentItem content
          tell "</"
          tell e.name
          tell ">"
