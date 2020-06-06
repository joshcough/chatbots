module RandomQuote
    ( def
    , Message
    ) where

import Prelude
import ChatBot.Models (ChannelName(..), ChatUserName(..), Quote(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Elmish (ComponentDef, DispatchMsgFn, ReactComponent, ReactElement, Transition(..), createElement')
import Network.Endpoints (getRandomQuote)
import Types (OpM)

data Message = GotQuote (Maybe Quote)

type UXQuote = { channel :: String, qid :: Int, body :: String, addedBy :: String }

type State = { quote :: Maybe Quote }

def :: ChannelName -> ComponentDef OpM Message State
def stream =
  { init: Transition { quote: Nothing } [ getNextRandomQuote ]
  , update
  , view: view
  }
  where
    getNextRandomQuote = GotQuote <$> getRandomQuote stream
    update s (GotQuote q) = s { quote = q } `Transition` [ afterSeconds 15.0 getNextRandomQuote ]

afterSeconds :: forall a . Number -> OpM a -> OpM a
afterSeconds n op = (liftAff <<< delay $ Milliseconds (n*1000.0)) *> op

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
