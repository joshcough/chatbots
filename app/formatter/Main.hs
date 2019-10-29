module Main where

import qualified Control.Foldl as Fold
import           Protolude     hiding (find, fold)
import           Turtle
import qualified Turtle        as T

findHaskell :: T.FilePath -> Shell T.FilePath
findHaskell = find (suffix ".hs")

allHaskellFiles :: Shell [T.FilePath]
allHaskellFiles = fold (findHaskell "src" <|> findHaskell "test" <|> findHaskell "app") Fold.list

stackExec :: MonadIO io => Text -> [Text] -> io ExitCode
stackExec prog args = proc "stack" (["exec", prog, "--"] ++ args) empty

main :: IO ()
main = sh $ do
  files <- fmap (either identity identity . toText) <$> allHaskellFiles
  stackExec "stylish-haskell" $ "--inplace" : files
  stackExec "brittany" $ ["--config-file", ".brittany.yaml", "--write-mode", "inplace"] ++ files
  proc "./.hlint.sh" [] empty
