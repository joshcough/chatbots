{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChatBot.WebSocket.ChatBotWS
  ( runBot
  ) where

import Protolude

import ChatBot.WebSocket.MessageProcessor (MessageProcessor(..), Sender(..), authorize, frontendListener)
import Config (Config, ConfigAndConnection(..))
import Control.Monad (forever)
import Data.String.Conversions (cs)
import Data.Text (Text)
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
    liftIO $ print msg
    maybe (throwError $ miscError "Server sent invalid message!") processMessage (parseRawIrcMsg msg)
