module Quotes
    ( def
    , Message
    ) where

import Prelude
import ChatBot.DatabaseModels (DbQuote(..))
import Control.Monad.Error.Class (class MonadError)
import Effect.Aff.Class (class MonadAff)
--import Elmish.HTML as R
import Elmish (ComponentDef, DispatchMsgFn, ReactComponent, ReactElement, Transition(..), createElement', pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, noData)
import Types (OpM)

data Message = GotQuotes (Array Quote)

type Quote = { channel :: String, qid :: Int, text :: String }

type State = { quotes :: Array Quote }

def :: ComponentDef OpM Message State
def =
  { init: Transition { quotes: [] } [ GotQuotes <$> getQuotes ]
  , update
  , view: view
  }
  where
    update s (GotQuotes qs) = pureUpdate s { quotes = qs }

foreign import view_ :: ReactComponent
  { quotes :: Array Quote
  }

view :: State -> DispatchMsgFn Message -> ReactElement
view s dispatch = createElement' view_ { quotes: s.quotes }

getQuotes :: forall m . MonadAff m => MonadError HttpException m => m (Array Quote)
getQuotes = map (map mkQuote) (httpJSON $ buildReq GET "http://localhost:8081/chatbot/quotes" noData)
  where
  mkQuote :: DbQuote -> Quote
  mkQuote (DbQuote r) = { channel: r.dbQuoteChannel, qid: r.dbQuoteQid, text: r.dbQuoteText }
