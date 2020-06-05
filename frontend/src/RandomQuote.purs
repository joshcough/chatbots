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

data Message = GetQuote String | GotQuote ChannelName (Maybe Quote)

type UXQuote = { channel :: String, qid :: Int, body :: String, addedBy :: String }

type State = { stream :: ChannelName, quote :: Maybe Quote }

def :: ChannelName -> ComponentDef OpM Message State
def initialStream =
  { init: Transition
            { stream: initialStream, quote: Nothing }
            [ GotQuote initialStream <$> getRandomQuote initialStream ]
  , update
  , view: view
  }
  where
    update s (GetQuote stream) =
      let c = ChannelName { _unChannelName : stream }
      in s `Transition` [ GotQuote c <$> getRandomQuote c ]
    update s (GotQuote c q) = pureUpdate s { stream = c, quote = q }

foreign import view_ :: ReactComponent {
    stream :: String
  , quote :: Nullable UXQuote
  }

view :: State -> DispatchMsgFn Message -> ReactElement
view s dispatch = createElement' view_
  { stream: g s.stream
  , quote: toNullable $ f <$> s.quote
  }
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
