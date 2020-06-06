module Main where

import Prelude

import ChatBot.Models (ChannelName(..))
import Components.Window as Window
import Concurrent.BoundedQueue as BQ
import Data.Array (mapMaybe, head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Elmish (Transition(..), pureUpdate)
import Elmish as Elmish
import Elmish.Component (ComponentDef)
import Elmish.React.DOM (empty)
import Network.Endpoints (getStreams) --, loginToken)
import Chat as Chat
import Quotes as Quotes
import RandomQuote as RandomQuote
import Types (Config, OpM, runOpM)
import URI.Extra.QueryPairs as QP
--import WsMain as WsMain
--import Effect.Console (log)

data View = Quotes | RandomQuote | Chat

main :: Effect Unit
main = launchAff_ $ do
  let config = {hostname:Nothing}
  streams <- runOpM config getStreams
  -- q2 <- BQ.new 100
  q <- BQ.new 100
  liftEffect $ do
    -- p <- Window.path_
    -- log p
    -- WsMain.main2 q2
    -- WsMain.main q
    mStream <- getStreamFromUrlParams
    mView <- getViewFromUrlParams
    let chan = ChannelName { _unChannelName : fromMaybe "daut" mStream }
        emptyDef = { init: Transition {} [], update: \_ _ -> pureUpdate {}, view: \_ _ -> empty }
    case mView of
      Just Chat        -> runComponent config $ Chat.def q chan
      Just Quotes      -> runComponent config $ Quotes.def streams chan
      Just RandomQuote -> runComponent config $ RandomQuote.def chan
      _                -> runDef emptyDef

runComponent :: forall m s . Config -> ComponentDef OpM m s -> Effect Unit
runComponent config d = runDef $ Elmish.nat (runOpM config) d

runDef :: forall m s . ComponentDef Aff m s -> Effect Unit
runDef d = Elmish.boot { domElementId: "app", def: d }

getStreamFromUrlParams :: Effect (Maybe String)
getStreamFromUrlParams = getArgFromParams "stream"

getViewFromUrlParams :: Effect (Maybe View)
getViewFromUrlParams = f <$> getArgFromParams "view"
  where f (Just "quotes")       = Just Quotes
        f (Just "random-quote") = Just RandomQuote
        f (Just "chat")         = Just Chat
        f _ = Nothing

-- TODO: this _could_ be an (Array String) if i feel like it. not sure yet.
getArgFromParams :: String -> Effect (Maybe String)
getArgFromParams p = getArgFromParams' <$> Window.getSearchParams
  where
  getArgFromParams' :: forall e. Either e (QP.QueryPairs String String) -> Maybe String
  getArgFromParams' (Left e) = Nothing
  getArgFromParams' (Right (QP.QueryPairs ps)) = head $ mapMaybe f ps
    where
    f (Tuple k (Just v)) | k == p = Just $ v
    f _ = Nothing

--  where
--  go :: forall msg state. String -> ComponentDef OpM msg state -> Effect { title :: String, view :: ReactElement }
--  go title def = do
--    render <- Elmish.construct (Elmish.nat runOpM def)
--    pure { title: title, view: render onError }
--
--  onError :: DispatchMsgFn Unit
--  onError = dispatchMsgFn log (const $ pure unit)
--
--  emptyDef = { init: Transition {} [], update: \_ _ -> pureUpdate {}, view: \_ _ -> empty }

--import Auth.Models (Login(..))
--import Components.Tabs as Tabs
--import Commands as Commands
--import Debug.Trace (spy)
--import Effect.Console (log)
--import Elmish.Dispatch (DispatchMsgFn, dispatchMsgFn)
--import Elmish.React (ReactElement)

-- hostname <- liftEffect $ lookupEnv "HOSTNAME"
