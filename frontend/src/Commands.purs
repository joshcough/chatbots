module Commands
    ( def
    , Message
    ) where

import Prelude
import ChatBot.DatabaseModels (DbCommand(..))
import Control.Monad.Error.Class (class MonadError)
import Effect.Aff.Class (class MonadAff)
--import Elmish.HTML as R
import Elmish (ComponentDef, DispatchMsgFn, ReactComponent, ReactElement, Transition(..), createElement', pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, noData)
import Types (OpM)

-- import JSX.Web.Core.Atoms.Layout.Grid (col, row)
-- import JSX.Web.Core.Atoms.Layout.Grid as Col

data Message = GotCommands (Array Command)

type Command = { channel :: String, name :: String, body :: String }

type State = { commands :: Array Command }

def :: ComponentDef OpM Message State
def =
  { init: Transition { commands: [] } [ GotCommands <$> getCommands ]
  , update
  , view: view
  }
  where
    update s (GotCommands cs) = pureUpdate s { commands = cs }

foreign import view_ :: ReactComponent
  { commands :: Array Command
--  , onInc :: JsCallback0
--  , onDec :: JsCallback0
  }

view :: State -> DispatchMsgFn Message -> ReactElement
view s dispatch = createElement' view_
                { commands: s.commands
--                , onInc: handle dispatch Inc
--                , onDec: handle dispatch Dec
                }

getCommands :: forall m . MonadAff m => MonadError HttpException m => m (Array Command)
getCommands = map (map mkCommand) (httpJSON $ buildReq GET "http://localhost:8081/chatbot/commands" noData)
  where
  mkCommand :: DbCommand -> Command
  mkCommand (DbCommand r) = { channel: r.dbCommandChannel, name: r.dbCommandName, body: r.dbCommandBody}
