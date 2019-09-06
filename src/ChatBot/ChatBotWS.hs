module ChatBot.ChatBotWS
  ( runBot
  ) where

import Protolude

import Control.Monad (forever)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Irc.RawIrcMsg (parseRawIrcMsg)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS

import ChatBot.MessageProcessor (MessageProcessor(..), authorize)
import Config (ConfigAndConnection(..), Config)
import Error (ChatBotError, miscError)
import Types (AppTEnv', runAppToIO)

twitchIrcUrl :: Text
twitchIrcUrl = "irc-ws.chat.twitch.tv"

runBot :: Config -> IO ()
runBot conf =
  withSocketsDo $ WS.runClient (cs twitchIrcUrl) 80 "/" $ \conn ->
    runAppToIO (ConfigAndConnection conf conn) app

type App = AppTEnv' ChatBotError IO ConfigAndConnection

app :: App ()
app = authorize >> twitchListener

twitchListener :: App ()
twitchListener = forever $ do
    ConfigAndConnection _ conn <- ask
    msg <- liftIO $ T.strip <$> WS.receiveData conn
    liftIO $ print msg
    maybe (throwError $ miscError "Server sent invalid message!") processMessage (parseRawIrcMsg msg)
