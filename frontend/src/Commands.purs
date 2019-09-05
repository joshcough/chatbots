module Commands
    ( def
    , Message
    ) where

import Prelude
import ChatBot.Models (ChannelName(..), Command(..))
import Control.Monad.Error.Class (class MonadError)
import Effect.Aff.Class (class MonadAff)
import Elmish (ComponentDef, DispatchMsgFn, ReactComponent, ReactElement, Transition(..), createElement', pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, jsonData)
import Types (OpM)

data Message = GotCommands (Array Command)

type UXCommand = { channel :: String, name :: String, body :: String }

type State = { commands :: Array Command }

def :: ChannelName -> ComponentDef OpM Message State
def channel =
  { init: Transition { commands: [] } [ GotCommands <$> getCommands channel ]
  , update
  , view: view
  }
  where
    update s (GotCommands cs) = pureUpdate s { commands = cs }

foreign import view_ :: ReactComponent { commands :: Array UXCommand }

view :: State -> DispatchMsgFn Message -> ReactElement
view s dispatch = createElement' view_ { commands: f <$> s.commands }
  where
  f (Command r) = { channel: channelName r.commandChannel, name: r.commandName, body: r.commandBody }
  channelName (ChannelName r) = r._unChannelName

getCommands :: forall m . MonadAff m => MonadError HttpException m => ChannelName -> m (Array Command)
getCommands c = httpJSON $ buildReq POST "http://localhost:8081/chatbot/commands" (jsonData c)
