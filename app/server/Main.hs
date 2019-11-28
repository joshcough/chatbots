module Main where

import GHC.IO.Encoding
import Init (runAppAndBot)
import Prelude (IO)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
  setLocaleEncoding utf8
  runAppAndBot
