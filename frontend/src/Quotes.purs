module Quotes
    ( def
    , Message
    ) where

import Prelude
import ChatBot.Models (Quote(..))
import Control.Monad.Error.Class (class MonadError)
import Effect.Aff.Class (class MonadAff)
import Elmish (ComponentDef, DispatchMsgFn, ReactComponent, ReactElement, Transition(..), createElement', pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, noData)
import Types (OpM)

data Message = GotQuotes (Array Quote)

type UXQuote = { channel :: String, qid :: Int, body :: String }

type State = { quotes :: Array Quote }

def :: ComponentDef OpM Message State
def =
  { init: Transition { quotes: [] } [ GotQuotes <$> getQuotes ]
  , update
  , view: view
  }
  where
    update s (GotQuotes qs) = pureUpdate s { quotes = qs }

foreign import view_ :: ReactComponent { quotes :: Array UXQuote }

view :: State -> DispatchMsgFn Message -> ReactElement
view s dispatch = createElement' view_ { quotes: f <$> s.quotes }
  where
  f (Quote r) = { channel: r.quoteChannel, qid: r.quoteQid, body: r.quoteName }

getQuotes :: forall m . MonadAff m => MonadError HttpException m => m (Array Quote)
getQuotes = httpJSON $ buildReq GET "http://localhost:8081/chatbot/quotes" noData

