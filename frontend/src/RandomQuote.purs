module RandomQuote
    ( def
    , Message
    ) where

import Prelude
import ChatBot.Models (ChannelName(..), ChatUserName(..), Quote(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Elmish (ComponentDef, DispatchMsgFn, ReactComponent, ReactElement, Transition(..), createElement', pureUpdate)
import Network.Endpoints (getRandomQuote)
import Types (OpM)

data Message = GotQuote (Maybe Quote)

type UXQuote = { channel :: String, qid :: Int, body :: String, addedBy :: String }

type State = { quote :: Maybe Quote }

def :: ChannelName -> ComponentDef OpM Message State
def stream =
  { init: Transition { quote: Nothing } [ GotQuote <$> getRandomQuote stream ]
  , update
  , view: view
  }
  where update s (GotQuote q) = pureUpdate s { quote = q }

foreign import view_ :: ReactComponent { quote :: Nullable UXQuote }

view :: State -> DispatchMsgFn Message -> ReactElement
view s dispatch = createElement' view_ { quote: toNullable $ f <$> s.quote }
  where
  f (Quote r) =
    { channel: channelName r.quoteChannel
    , qid: r.quoteQid
    , body: r.quoteBody
    , addedBy: h $ r.quoteUser
    }
  channelName (ChannelName r) = r._unChannelName
  g (ChannelName r) = r._unChannelName
  h (ChatUserName r) = r.cunName
