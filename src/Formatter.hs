module Formatter (formatHaskell) where

import qualified Control.Foldl as Fold
import           Protolude     hiding (find, fold)
import           Turtle        hiding (f)
import qualified Turtle        as T

allHaskellFiles :: Shell [T.FilePath]
allHaskellFiles = fold (f "src" <|> f "test" <|> f "app") Fold.list where f = find (suffix ".hs")

stackExec :: MonadIO io => Text -> [Text] -> io ExitCode
stackExec prog args = proc "stack" (["exec", prog, "--"] ++ args) empty

formatHaskell :: IO ()
formatHaskell = sh $ do
  files <- fmap (either identity identity . toText) <$> allHaskellFiles
  stackExec "stylish-haskell" $ "--inplace" : files
  stackExec "brittany" $ ["--config-file", ".brittany.yaml", "--write-mode", "inplace"] ++ files
  proc "./.hlint.sh" [] empty
