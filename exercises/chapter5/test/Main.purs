module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Picture (Point(..), Shape(..), Picture, Bounds(..), bounds, showBounds, shapeBounds)
import Data.Maybe (Maybe(..))

circle :: Shape
circle = Circle (Point { x: 0.0, y: 0.0 }) 10.0

rectangle :: Shape
rectangle = Rectangle (Point { x: 10.0, y: 10.0 }) 10.0 10.0

picture :: Picture
picture = [circle, rectangle]

main :: Effect Unit
main = log (showBounds (bounds picture))

fact :: Int -> Int
fact 0 = 1
fact n = fact (n - 1) * n

factTail :: Int -> Int
factTail n = factTail' n 1
  where
    factTail' 0 acc = acc
    factTail' n' acc = factTail' (n' - 1) (acc * n')

binomialCoefficient :: Int -> Int -> Int
binomialCoefficient n k
  | n < k = 0
  | otherwise = (fact n) / ((fact k) * fact (n - k))

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

sameCity :: Person -> Person -> Boolean
-- sameCity ::
--   forall r1 r2 r3 r4 eq. Eq eq =>
--     { address :: { city :: eq | r1 } | r2 } ->
--     { address :: { city :: eq | r3 } | r4 } ->
--     Boolean
sameCity { address: { city: cityA } } { address: { city: cityB } } = cityA == cityB

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton default _ = default

double :: Shape -> Shape
double (Circle origin r) = Circle origin (r * 2.0)
double (Rectangle origin a b) = Rectangle origin (a * 2.0) (b * 2.0)
double (Line start (Point { x, y })) = Line start (Point { x: x * 2.0, y: y * 2.0 })
double text = text

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text
shapeText _ = Nothing

shapeArea :: Shape -> Number
shapeArea shape = boundsArea (shapeBounds shape)
  where
    boundsArea (Bounds { top, left, bottom, right }) = (right - left) * (top - bottom)
