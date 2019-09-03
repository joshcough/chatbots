module Commands
    ( def
    , Message
    ) where

import Prelude
import ChatBot.Models (Command(..))
import Control.Monad.Error.Class (class MonadError)
import Effect.Aff.Class (class MonadAff)
import Elmish (ComponentDef, DispatchMsgFn, ReactComponent, ReactElement, Transition(..), createElement', pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, noData)
import Types (OpM)

data Message = GotCommands (Array Command)

type UXCommand = { channel :: String, name :: String, body :: String }

type State = { commands :: Array Command }

def :: ComponentDef OpM Message State
def =
  { init: Transition { commands: [] } [ GotCommands <$> getCommands ]
  , update
  , view: view
  }
  where
    update s (GotCommands cs) = pureUpdate s { commands = cs }

foreign import view_ :: ReactComponent { commands :: Array UXCommand }

view :: State -> DispatchMsgFn Message -> ReactElement
view s dispatch = createElement' view_ { commands: f <$> s.commands }
  where
  f (Command r) = { channel: r.commandChannel, name: r.commandName, body: r.commandBody}

getCommands :: forall m . MonadAff m => MonadError HttpException m => m (Array Command)
getCommands = httpJSON $ buildReq GET "http://localhost:8081/chatbot/commands" noData
