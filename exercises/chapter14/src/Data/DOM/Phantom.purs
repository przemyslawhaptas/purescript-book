module Data.DOM.Phantom
  ( Element
  , Attribute
  , Content
  , AttributeKey
  , Size
  , Empty
  , NotEmpty
  , class IsValue
  , toValue
  , class IsNotEmpty

  , a
  , p
  , img

  , href
  , _class
  , src
  , width
  , height

  , attribute, (:=)
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

newtype AttributeKey :: forall k1 k2. k1 -> k2 -> Type
newtype AttributeKey value empty = AttributeKey String

data Size
  = Pixels Int
  | Percent Int

data Empty
data NotEmpty

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

class IsNotEmpty :: forall k. k -> Constraint
class IsNotEmpty a

instance notEmptyIsNotEmpty :: IsNotEmpty NotEmpty

class IsEmpty :: forall k. k -> Constraint
class IsEmpty a

instance emptyIsEmpty :: IsEmpty Empty

attribute :: forall a b. IsValue a => IsNotEmpty b => AttributeKey a b -> a -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: Just $ toValue value
  }

emptyAttribute :: forall a b. IsEmpty b => AttributeKey a b -> a -> Attribute
emptyAttribute (AttributeKey key) value = Attribute
  { key: key
  , value: Nothing
  }

infix 4 attribute as :=

a :: Array Attribute -> Array Content -> Element
a attribs content = element "a" attribs (Just content)

p :: Array Attribute -> Array Content -> Element
p attribs content = element "p" attribs (Just content)

img :: Array Attribute -> Element
img attribs = element "img" attribs Nothing

href :: AttributeKey String NotEmpty
href = AttributeKey "href"

_class :: AttributeKey String NotEmpty
_class = AttributeKey "class"

src :: AttributeKey String NotEmpty
src = AttributeKey "src"

width :: AttributeKey Size NotEmpty
width = AttributeKey "width"

height :: AttributeKey Size NotEmpty
height = AttributeKey "height"

disabled :: AttributeKey Unit Empty
disabled = AttributeKey "disabled"

checked :: AttributeKey Unit Empty
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
