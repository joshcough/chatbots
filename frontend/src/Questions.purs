module Questions
    ( def
    , Message
    ) where

import Prelude
import ChatBot.Models (ChannelName(..), Question(..))
import Elmish (ComponentDef, DispatchMsg, DispatchMsgFn, JsCallback, ReactComponent, ReactElement, Transition(..), createElement', handle, pureUpdate)
import Network.Endpoints (getQuestions)
import Types (OpM)

data Message = GetQuestions String | GotQuestions ChannelName (Array Question)

type UXQuestion = { channel :: String, qid :: Int, body :: String }

type State = { streams :: Array ChannelName, stream :: ChannelName, questions :: Array Question }

def :: Array ChannelName -> ChannelName -> ComponentDef OpM Message State
def streams stream =
  { init: Transition { streams, stream, questions: [] } [ GotQuestions stream <$> getQuestions stream ]
  , update
  , view: view
  }
  where
    update s (GetQuestions stream) =
      let c = ChannelName { _unChannelName : stream }
      in s `Transition` [ GotQuestions c <$> getQuestions c ]
    update s (GotQuestions c qs) = pureUpdate s { stream = c, questions = qs }

foreign import view_ :: ReactComponent {
    selectStream :: JsCallback (String -> DispatchMsg)
  , streams :: Array String
  , stream :: String
  , questions :: Array UXQuestion
  }

view :: State -> DispatchMsgFn Message -> ReactElement
view s dispatch = createElement' view_
  { selectStream: handle dispatch GetQuestions
  , streams: g <$> s.streams
  , stream: g s.stream
  , questions: f <$> s.questions
  }
  where
  f (Question r) = { channel: channelName r.questionChannel, qid: r.questionQid, body: r.questionBody }
  channelName (ChannelName r) = r._unChannelName
  g (ChannelName r) = r._unChannelName
