{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChatBot.WebSocket.ChatBotWS
  ( runBot
  , runImporter
  , runInserter
  ) where

import           Prelude                            (error)
import           Protolude

import           ChatBot.Config                     (ChannelName, mkChannelName)
import           ChatBot.Models                     (trollabotUser)
import           ChatBot.Storage                    (QuotesDb (..))
import           ChatBot.WebSocket.MessageProcessor (MessageImporter (..), MessageProcessor (..),
                                                     Sender (..), authorize, connectTo,
                                                     disconnectFrom, frontendListener, say)
import           Config                             (Config, ConfigAndConnection (..))
import           Control.Concurrent                 (threadDelay)
import           Control.Monad                      (forever)
import           Data.String.Conversions            (cs)
import           Data.Text                          (Text, lines)
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
runBot conf = withSocketsDo $ WS.runClient (cs twitchIrcUrl) 80 "/" $ \conn ->
  do
      let c = ConfigAndConnection conf conn
      _ <- forkIO $ runAppTAndThrow c frontendListener
      runAppTAndThrow c $ authorize >> twitchListener
  -- if there is some blip, wait a second and try again.
    `catch` (\(e :: SomeException) -> loop e)
    `catch` (\(e :: SomeAsyncException) -> loop e)
 where
  loop :: Show e => e -> IO ()
  loop e = print e >> threadDelay 1000000 >> runBot conf

twitchListener :: App ()
twitchListener = forever $ do
  ConfigAndConnection _ conn <- ask
  msg <- liftIO $ T.strip <$> WS.receiveData conn
  maybe (throwError $ miscError "Server sent invalid message!") processMessage (parseRawIrcMsg msg)

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
