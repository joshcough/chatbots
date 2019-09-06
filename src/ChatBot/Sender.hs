module ChatBot.Sender
  ( Sender(..)
  ) where

import Protolude

import Irc.RawIrcMsg (RawIrcMsg(..), renderRawIrcMsg)
import qualified Network.WebSockets as WS
import Config (ConfigAndConnection(..))
import Error (ChatBotError)
import Types (AppTEnv')

type App = AppTEnv' ChatBotError IO ConfigAndConnection

class Sender m where
  send :: RawIrcMsg -> m ()

instance Sender App where
  send msg = do
    ConfigAndConnection _ conn <- ask
    liftIO $ WS.sendTextData conn (renderRawIrcMsg msg)
