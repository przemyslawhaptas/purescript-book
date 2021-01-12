module FileOperations where

import Prelude

import Data.Path (Path, ls, isDirectory, size, filename, root)
import Data.Array (concatMap, (:), filter, head)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(), fromMaybe)

allFiles :: Path -> Array Path
allFiles path = path : concatMap allFiles (ls path)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles = filter (not <<< isDirectory) <<< allFiles'

maxInt :: Int
maxInt = 2147483647

findLargestAndSmallest :: Path -> { largest :: Path, smallest :: Path }
findLargestAndSmallest rootPath =
  foldl updateExtrema { largest: rootPath, smallest: rootPath } $ onlyFiles rootPath
  where
    updateExtrema ({ largest, smallest }) path =
      { largest: if (isLarger largest path) then largest else path
      , smallest: if (isSmaller smallest path) then smallest else path
      }
    compare default f a b = f (fromMaybe default (size a)) (fromMaybe default (size b))
    isLarger a b = compare 0 (>) a b
    isSmaller a b = compare maxInt (<) a b


whereIs :: String -> Maybe Path
whereIs pathName = head $ whereIs' root pathName
  where
    whereIs' path name = do
      file <- ls path
      if filename file == name
        then pure path
        else whereIs' file name
