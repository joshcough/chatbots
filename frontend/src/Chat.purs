module Chat
    ( def
    , Message
    ) where

import Prelude
import ChatBot.IrcMsg as Irc
import ChatBot.Models (ChannelName, ChatMessage)
import Concurrent.BoundedQueue as BQ
import Data.Array (snoc, takeEnd)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Tuple (snd)
import Effect.Aff.Class (liftAff)
import Elmish (ComponentDef, DispatchMsgFn, ReactComponent, ReactElement, Transition(..), createElement')
import Elmish.HTML as R
import Types (OpM)

data Message = GotMessage ChatMessage

type State = { messages :: Array ChatMessage }

def :: BQ.BoundedQueue ChatMessage -> ChannelName -> ComponentDef OpM Message State
def q channel =
  { init: { messages: [] } `Transition` [ getMessage ]
  , update
  , view: view
  }
  where
    update s (GotMessage cm) =
      s { messages = takeEnd 100 $ snoc s.messages cm } `Transition` [ getMessage ]
    getMessage = GotMessage <$> liftAff (BQ.read q)

type UIMessage = { user :: String, body :: Array ReactElement }

foreign import view_ :: ReactComponent { messages :: Array UIMessage }

view :: State -> DispatchMsgFn Message -> ReactElement
view s dispatch = createElement' view_ { messages: toUIMessage <$> s.messages }
  where
  toUIMessage cm =
    { user: unwrap >>> _.cmUser >>> unwrap >>> _.cuUserName >>> unwrap >>> _.cunName $ cm
    , body: unwrap >>> _.cmRawMessage >>> Irc.tagsAndSections >>> snd >>> map f $ cm
    }
  f :: Either String String -> ReactElement
  f (Left s) = R.text s
  f (Right link) = R.img {src : link}
