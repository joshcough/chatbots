module Main where

import Prelude (IO)
import Init (runAppAndBot)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = runAppAndBot
