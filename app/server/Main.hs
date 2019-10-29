module Main where

import           Init    (runAppAndBot)
import           Prelude (IO)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = runAppAndBot
