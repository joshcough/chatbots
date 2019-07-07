
module Init where

import Protolude

import           Control.Exception           (bracket)
import qualified Data.Pool                   as Pool

import           ChatBot.ChatBotWS           (runBot)
import           Config                      (Config (..), acquireConfig)

-- | An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit
runApp :: IO ()
runApp = bracket acquireConfig shutdownApp runBot

-- | Takes care of cleaning up 'Config' resources
shutdownApp :: Config -> IO ()
shutdownApp Config {..} = do
    Pool.destroyAllResources _configPool
    pure ()
