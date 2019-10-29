#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-} 

import Turtle
import qualified Control.Foldl as Fold

findHaskell = find (suffix ".hs")
allHaskellFiles = fold (findHaskell "src" <|> findHaskell "test" <|> findHaskell "app") Fold.list
stackExec prog args = proc "stack" (["exec", prog, "--"] ++ args) empty

main = sh $ do
  files <- fmap (either id id . toText) <$> allHaskellFiles
  stackExec "stylish-haskell" $ "--inplace" : files
  stackExec "brittany" $ ["--config-file", ".brittany.yaml", "--write-mode", "inplace"] ++ files
  proc "./.hlint.sh" [] empty
  stackExec "weeder" [".", "--build"]
  runTests

runTests = sh $ do
  export "ROLLBAR_TOKEN" "undefined"
  export "ROLLBAR_ENVIRONMENT" "undefined"
  export "CHATBOT_NICK" "undefined"
  export "CHATBOT_PASS" "undefined"
  export "ROLLBAR_TOKEN" "undefined"
  proc "stack" ["test"] empty
