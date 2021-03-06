
module Init where

import           Protolude

import           Control.Concurrent                    (forkIO)
import           Control.Exception                     (bracket)
import qualified Data.Pool                             as Pool
import           Network.Wai                           (Application)
import           Network.Wai.Handler.Warp              (run)
import           Network.Wai.Middleware.Cors           (simpleCors)
import           Network.Wai.Middleware.Servant.Errors (errorMwDefJson)

import           Api                                   (app)
import           ChatBot.WebSocket.ChatBotWS           (runBot)
import           Config                                (Config (..), acquireConfig)

-- | An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit
runAppAndBot :: IO ()
runAppAndBot = bracket acquireConfig shutdownApp runApp'
 where
  runApp' config = do
      -- run the chat bot
    _ <- forkIO $ runBot config
    -- run the servant app
    run (_configPort config) . errorMwDefJson =<< initialize config

botOnly :: IO ()
botOnly = bracket acquireConfig shutdownApp runBot

{-
importer :: Text -> IO ()
importer cn = bracket acquireConfig shutdownApp (runImporter $ mkChannelName cn)

inserter :: Text -> IO ()
inserter cn = bracket acquireConfig shutdownApp (runInserter $ mkChannelName cn)
-}

appOnly :: IO ()
appOnly = bracket acquireConfig shutdownApp runApp'
  where runApp' config = run (_configPort config) =<< initialize config

-- | Takes care of cleaning up 'Config' resources
shutdownApp :: Config -> IO ()
shutdownApp Config {..} = do
  Pool.destroyAllResources _configPool
  pure ()

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Application
initialize = pure . simpleCors . app
