module Data.DOM.Smart
  ( Element
  , Attribute
  , Content
  , AttributeKey

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

newtype AttributeKey = AttributeKey String

attribute :: AttributeKey -> String -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: Just value
  }

emptyAttribute :: AttributeKey -> Attribute
emptyAttribute (AttributeKey key) = Attribute
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

html :: Array Attribute -> Array Content -> Element
html attribs content = element "html" attribs (Just content)

head :: Array Attribute -> Array Content -> Element
head attribs content = element "head" attribs (Just content)

body :: Array Attribute -> Array Content -> Element
body attribs content = element "body" attribs (Just content)

meta :: Array Attribute -> Element
meta attribs = element "meta" attribs Nothing

title :: Array Attribute -> Array Content -> Element
title attribs content = element "title" attribs (Just content)

link :: Array Attribute -> Element
link attribs = element "link" attribs Nothing

script :: Array Attribute -> Array Content -> Element
script attribs content = element "script" attribs (Just content)

href :: AttributeKey
href = AttributeKey "href"

_class :: AttributeKey
_class = AttributeKey "class"

src :: AttributeKey
src = AttributeKey "src"

width :: AttributeKey
width = AttributeKey "width"

height :: AttributeKey
height = AttributeKey "height"

lang :: AttributeKey
lang = AttributeKey "lang"

charset :: AttributeKey
charset = AttributeKey "charset"

rel :: AttributeKey
rel = AttributeKey "rel"

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

-- not quite valid at this point
exampleHtml :: String
exampleHtml = render $
  html [ lang := "en" ]
    [ (elem $ head []
      [ (elem $ meta [ charset := "utf-8" ])
      , (elem $ title [] [ text "Example HTML" ])
      , (elem $ link [ rel := "stylesheet", href := "style.css" ])
      , (elem $ script [ src := "script.js" ] [])
      ])
    , (elem $ body [] [ text "Page Content" ])
    ]
