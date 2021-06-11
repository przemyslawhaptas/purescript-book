module Files where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn4, Fn3, runFn4, runFn3)
import Data.Traversable (traverse)
import Effect (Effect)
import Types (Async)

foreign import data FS :: Effect

type ErrorCode = String

type FilePath = String

foreign import readFileImpl :: Fn3 FilePath
                                   (String -> Effect Unit)
                                   (ErrorCode -> Effect Unit)
                                   (Effect Unit)

foreign import writeFileImpl :: Fn4 FilePath
                                    String
                                    (Effect Unit)
                                    (ErrorCode -> Effect Unit)
                                    (Effect Unit)

readFile :: FilePath -> (Either ErrorCode String -> Effect Unit) -> Effect Unit
readFile path k = runFn3 readFileImpl path (k <<< Right) (k <<< Left)

writeFile :: FilePath -> String -> (Either ErrorCode Unit -> Effect Unit) -> Effect Unit
writeFile path text k = runFn4 writeFileImpl path text (k $ Right unit) (k <<< Left)

readFileCont :: FilePath -> Async (Either ErrorCode String)
readFileCont path = ContT $ readFile path

writeFileCont :: FilePath -> String -> Async (Either ErrorCode Unit)
writeFileCont path text = ContT $ writeFile path text

copyFileCont :: FilePath -> FilePath -> Async (Either ErrorCode Unit)
copyFileCont src dest = do
  e <- readFileCont src
  case e of
    Left err -> pure $ Left err
    Right content -> writeFileCont dest content

concatFilesCont :: FilePath -> FilePath -> FilePath -> Async (Either ErrorCode Unit)
concatFilesCont srcA srcB dest = do
  resultA <- readFileCont srcA
  resultB <- readFileCont srcB
  case (<>) <$> resultA <*> resultB of
    Right contentAB -> writeFileCont dest contentAB
    Left err -> pure $ Left err

readFileContEx :: FilePath -> ExceptT ErrorCode Async String
readFileContEx path = ExceptT $ readFileCont path

writeFileContEx :: FilePath -> String -> ExceptT ErrorCode Async Unit
writeFileContEx path text = ExceptT $ writeFileCont path text

copyFileContEx :: FilePath -> FilePath -> ExceptT ErrorCode Async Unit
copyFileContEx src dest = do
  content <- readFileContEx src
  writeFileContEx dest content

concatFilesContEx :: FilePath -> FilePath -> FilePath -> ExceptT ErrorCode Async Unit
concatFilesContEx srcA srcB dest = do
  contentA <- readFileContEx srcA
  contentB <- readFileContEx srcB
  writeFileContEx dest (contentA <> contentB)

concatenateMany :: Array FilePath -> FilePath -> ExceptT ErrorCode Async (Array Unit)
concatenateMany srcs dest = traverse (\src -> concatFilesContEx dest src dest) srcs

test :: Effect Unit
test = do
  runContT (runExceptT (concatenateMany ["1.txt", "2.txt", "3.txt"] "dest.txt")) mempty
