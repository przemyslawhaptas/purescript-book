module Data.DOM.PhantomAnotherTake
  ( Element
  , Attribute
  , Content
  , AttributeKey
  , Size
  , Empty
  , NotEmpty
  , class IsValue
  , toValue

  , a
  , p
  , img

  , href
  , _class
  , src
  , width
  , height

  , attribute, (:=)
  , emptyAttribute
  , text
  , elem

  , pixels
  , percent

  , render
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Array Content)
  }

data Content
  = TextContent String
  | ElementContent Element

newtype Attribute = Attribute
  { key          :: String
  , value        :: Maybe String
  }

newtype AttributeKey :: forall k. k -> Type
newtype AttributeKey a = AttributeKey String

data Size
  = Pixels Int
  | Percent Int

data Empty
data NotEmpty :: forall k. k -> Type
data NotEmpty a

pixels :: Int -> Size
pixels = Pixels

percent :: Int -> Size
percent = Percent

element :: String -> Array Attribute -> Maybe (Array Content) -> Element
element name attribs content = Element
  { name:      name
  , attribs:   attribs
  , content:   content
  }

text :: String -> Content
text = TextContent

elem :: Element -> Content
elem = ElementContent

class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = identity

instance intIsValue :: IsValue Int where
  toValue = show

instance sizeIsValue :: IsValue Size where
  toValue (Pixels px) = show px <> "px"
  toValue (Percent perc) = show perc <> "%"

attribute :: forall a. IsValue a => AttributeKey (NotEmpty a) -> a -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: Just $ toValue value
  }

infix 4 attribute as :=

emptyAttribute :: AttributeKey Empty -> Attribute
emptyAttribute (AttributeKey key) = Attribute
  { key: key
  , value: Nothing
  }

a :: Array Attribute -> Array Content -> Element
a attribs content = element "a" attribs (Just content)

p :: Array Attribute -> Array Content -> Element
p attribs content = element "p" attribs (Just content)

img :: Array Attribute -> Element
img attribs = element "img" attribs Nothing

href :: AttributeKey (NotEmpty String)
href = AttributeKey "href"

_class :: AttributeKey (NotEmpty String)
_class = AttributeKey "class"

src :: AttributeKey (NotEmpty String)
src = AttributeKey "src"

width :: AttributeKey (NotEmpty Size)
width = AttributeKey "width"

height :: AttributeKey (NotEmpty Size)
height = AttributeKey "height"

disabled :: AttributeKey Empty
disabled = AttributeKey "disabled"

checked :: AttributeKey Empty
checked = AttributeKey "checked"

render :: Element -> String
render (Element e) =
    "<" <> e.name <>
    " " <> joinWith " " (map renderAttribute e.attribs) <>
    renderContent e.content
  where
    renderAttribute :: Attribute -> String
    renderAttribute (Attribute { key, value: Just value }) = key <> "=\"" <> value <> "\""
    renderAttribute (Attribute { key, value: Nothing }) = key

    renderContent :: Maybe (Array Content) -> String
    renderContent Nothing = " />"
    renderContent (Just content) =
        ">" <> joinWith "" (map renderContentItem content) <>
        "</" <> e.name <> ">"
      where
        renderContentItem :: Content -> String
        renderContentItem (TextContent s) = s
        renderContentItem (ElementContent e') = render e'
