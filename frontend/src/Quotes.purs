module Quotes
    ( def
    , Message
    ) where

import Prelude
import ChatBot.Models (ChannelName(..), ChatUserName(..), Quote(..))
import Elmish (ComponentDef, DispatchMsg, DispatchMsgFn, JsCallback, ReactComponent, ReactElement, Transition(..), createElement', handle, pureUpdate)
import Network.Endpoints (getQuotes)
import Types (OpM)

data Message = GetQuotes String | GotQuotes ChannelName (Array Quote)

type UXQuote = { channel :: String, qid :: Int, body :: String, addedBy :: String }

type State = { streams :: Array ChannelName, stream :: ChannelName, quotes :: Array Quote }

def :: Array ChannelName -> ChannelName -> ComponentDef OpM Message State
def streams initialStream =
  { init: Transition
            { streams, stream: initialStream, quotes: [] }
            [ GotQuotes initialStream <$> getQuotes initialStream ]
  , update
  , view: view
  }
  where
    update s (GetQuotes stream) =
      let c = ChannelName { _unChannelName : stream }
      in s `Transition` [ GotQuotes c <$> getQuotes c ]
    update s (GotQuotes c qs) = pureUpdate s { stream = c, quotes = qs }

foreign import view_ :: ReactComponent {
    selectStream :: JsCallback (String -> DispatchMsg)
  , streams :: Array String
  , stream :: String
  , quotes :: Array UXQuote
  }

view :: State -> DispatchMsgFn Message -> ReactElement
view s dispatch = createElement' view_
  { selectStream: handle dispatch GetQuotes
  , streams: g <$> s.streams
  , stream: g s.stream
  , quotes: f <$> s.quotes
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
