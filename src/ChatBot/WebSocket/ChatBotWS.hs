{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChatBot.WebSocket.ChatBotWS
  ( runBot
  ) where

import           Protolude

import           ChatBot.Config                     (mkChannelName)
import           ChatBot.WebSocket.MessageProcessor (MessageProcessor (..), Sender (..), authorize,
                                                     connectTo, frontendListener)
import           Config                             (Config, ConfigAndConnection (..))
import           Data.String.Conversions            (cs)
import qualified Data.Text                          as T
import           Error                              (ChatBotError, miscError)
import           Irc.RawIrcMsg                      (parseRawIrcMsg, renderRawIrcMsg)
import           Network.Socket                     (withSocketsDo)
import qualified Network.WebSockets                 as WS
import           Types                              (AppTEnv', runAppTAndThrow)

twitchIrcUrl :: Text
twitchIrcUrl = "irc-ws.chat.twitch.tv"

type App = AppTEnv' ChatBotError IO ConfigAndConnection

instance Sender App where
  send msg = do
    ConfigAndConnection _ conn <- ask
    liftIO $ WS.sendTextData conn (renderRawIrcMsg msg)

runBot :: Config -> IO ()
runBot conf = withSocketsDo $ WS.runClient (cs twitchIrcUrl) 80 "/" $ \conn -> do
  let c = ConfigAndConnection conf conn
  _ <- forkIO $ f $ runAppTAndThrow c frontendListener
  f $ runAppTAndThrow c $ authorize >> connectDaut >> twitchListener
 where
  -- if there is some blip, wait a second and try again.
  f m =
    m `catch` (\(e :: SomeException) -> loop e m) `catch` (\(e :: SomeAsyncException) -> loop e m)
  loop e m = print e >> threadDelay 1000000 >> m

  -- TODO: we should have some notion of what streams we are connected to
  -- probably in the database, get all those, and connect to them.
  connectDaut = connectTo (mkChannelName "daut")


twitchListener :: App ()
twitchListener = forever $ do
  ConfigAndConnection _ conn <- ask
  msg <- liftIO $ T.strip <$> WS.receiveData conn
  maybe (throwError $ miscError "Server sent invalid message!") processMessage (parseRawIrcMsg msg)

{-
runImporter :: ChannelName -> Config -> IO ()
runImporter chan conf = withSocketsDo $ WS.runClient (cs twitchIrcUrl) 80 "/" $ \conn ->
  runAppTAndThrow (ConfigAndConnection conf conn) $ do
    let daut = mkChannelName "daut"
    authorize
    connectTo daut
    liftIO $ threadDelay 5000000
    forM_ [0 .. 561] $ \n -> do
      say daut $ "!quote" <> show (n :: Int)
      msg <- liftIO $ T.strip <$> WS.receiveData conn
      maybe
        (throwError $ miscError "Server sent invalid message!")
        (processImportMessage chan)
        (parseRawIrcMsg msg)
      liftIO $ threadDelay 2000000
    disconnectFrom daut

runInserter :: ChannelName -> Config -> IO ()
runInserter cn conf = runAppTAndThrow (ConfigAndConnection conf $ error "ununsed") $ do
  f <- liftIO $ readFile "./data/quotes_no_ids.txt"
  forM_ (lines f) $ insertQuote cn trollabotUser
-}
