{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChatBot.WebSocket.ChatBotWS
  ( runBot
  , runImporter
  , runInserter
  ) where

import Protolude
import Prelude (error)

import ChatBot.Config (ChannelName(..))
import ChatBot.Storage (QuotesDb(..))
import ChatBot.WebSocket.MessageProcessor
  ( MessageImporter(..)
  , MessageProcessor(..)
  , Sender(..)
  , authorize
  , connectTo
  , disconnectFrom
  , frontendListener
  , say
  )
import Config (Config, ConfigAndConnection(..))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.String.Conversions (cs)
import Data.Text (Text, lines)
import qualified Data.Text as T
import Error (ChatBotError, miscError)
import Irc.RawIrcMsg (parseRawIrcMsg, renderRawIrcMsg)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Types (AppTEnv', runAppToIO)

twitchIrcUrl :: Text
twitchIrcUrl = "irc-ws.chat.twitch.tv"

type App = AppTEnv' ChatBotError IO ConfigAndConnection

instance Sender App where
  send msg = do
    ConfigAndConnection _ conn <- ask
    liftIO $ WS.sendTextData conn (renderRawIrcMsg msg)

runBot :: Config -> IO ()
runBot conf =
  withSocketsDo $ WS.runClient (cs twitchIrcUrl) 80 "/" $ \conn -> do
    let f = runAppToIO (ConfigAndConnection conf conn)
    _ <- forkIO $ f frontendListener
    f $ authorize >> twitchListener

twitchListener :: App ()
twitchListener = forever $ do
    ConfigAndConnection _ conn <- ask
    msg <- liftIO $ T.strip <$> WS.receiveData conn
    maybe (throwError $ miscError "Server sent invalid message!")
          processMessage
          (parseRawIrcMsg msg)

runImporter :: ChannelName -> Config -> IO ()
runImporter chan conf =
  withSocketsDo $ WS.runClient (cs twitchIrcUrl) 80 "/" $ \conn ->
    runAppToIO (ConfigAndConnection conf conn) $ do
        let daut = ChannelName "#daut"
        authorize
        connectTo daut
        liftIO $ threadDelay 5000000
        forM_ [0..561] $ \n -> do
            say daut $ "!quote" <> show (n::Int)
            msg <- liftIO $ T.strip <$> WS.receiveData conn
            maybe (throwError $ miscError "Server sent invalid message!")
                  (processImportMessage chan)
                  (parseRawIrcMsg msg)
            liftIO $ threadDelay 2000000
        disconnectFrom daut

runInserter :: ChannelName -> Config -> IO ()
runInserter cn conf =
    runAppToIO (ConfigAndConnection conf $ error "ununsed") $ do
        f <- liftIO $ readFile "./data/quotes_no_ids.txt"
        forM_ (lines f) $ insertQuote cn
