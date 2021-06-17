module Test.Main where

import Prelude

import Data.Array (all, intersect, length, replicate, sortBy)
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.List (List(..), fromFoldable)
import Data.Maybe.First (First(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Merge (mergeWith, mergePoly, merge)
import Sorted (sorted)
import Test.QuickCheck (Result(..), quickCheck, (<?>))
import Tree (Tree, member, insert, toArray, anywhere)

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true

isSubarrayOf :: forall a. (Eq a) => Array a -> Array a -> Boolean
isSubarrayOf xs ys = xs `intersect` ys == xs

int :: Int -> Int
int = identity

ints :: Array Int -> Array Int
ints = identity

bools :: Array Boolean -> Array Boolean
bools = identity

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = identity

treeOfNumber :: Tree Number -> Tree Number
treeOfNumber = identity

treeOfInt :: Tree Int -> Tree Int
treeOfInt = identity

allSuccessful :: List Result -> Boolean
allSuccessful results = foldMap resultToFirstMaybe results == First Nothing
  where
    resultToFirstMaybe Success = First Nothing
    resultToFirstMaybe (Failed error) = First (Just error)

main :: Effect Unit
main = do
  -- Tests for module 'Merge'

  quickCheck $ \xs ys ->
    let
      result = merge (sorted xs) (sorted ys)
    in
      isSorted result <?> show result <> " is not sorted"

  quickCheck $ \xs ys ->
    let
      result = merge xs ys
    in
      xs `isSubarrayOf` result <?> show result <> " is not a subarray of " <> show xs

  quickCheck $ \xs ->
    let
      result = merge xs []
    in
      result == xs <?> show xs <> " has changed after merging an empty array"

  quickCheck $ \xs ys -> isSorted $ ints $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> ints xs `isSubarrayOf` mergePoly xs ys

  quickCheck $ \xs ys -> isSorted $ bools $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> bools xs `isSubarrayOf` mergePoly xs ys

  quickCheck $ \xs ys f ->
    isSorted $ map f $
      mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)

  quickCheck $ \xs ys f -> xs `isSubarrayOf` mergeWith (intToBool f) xs ys

  quickCheck $ \xs ys zs f ->
    mergeWith (intToBool f) xs (mergeWith f ys zs) == mergeWith f (mergeWith f xs ys) zs

  -- Tests for module 'Tree'

  quickCheck $ \t a -> member a $ insert a $ treeOfNumber t
  quickCheck $ \t xs -> isSorted $ toArray $ foldr insert t $ ints xs

  quickCheck $ \f g t ->
    anywhere (\s -> f s || g s) t ==
      anywhere f (treeOfNumber t) || anywhere g t

  quickCheck $ \t a bs ->
    member a $ foldr insert (insert (int a) $ treeOfInt t) (ints bs)


  -- Tests for module 'Data.Array'

  quickCheck $ \n a ->
    let
      result = replicate n a
      resultLength = length (ints result)
    in
      (if n < 0 then resultLength == 0 else resultLength == n)
        <?> show result <> " is not of length " <> show n

  quickCheck $ \n a ->
    let
      result = replicate n a
    in
      all (eq a) (ints result)
        <?> show result <> " doesn't contain only elements equal " <> show a
