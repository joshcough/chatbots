module Main where

import Prelude

import Data.Array (mapMaybe, head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Elmish as Elmish
import Elmish.Component (ComponentDef)
import Elmish.Dispatch (DispatchMsgFn, dispatchMsgFn)
import Elmish.React (ReactElement)
import Elmish.React.DOM (empty)
import Components.Tabs as Tabs
import Commands as Commands
import Quotes as Quotes
import Types (OpM, runOpM)
import Debug.Trace (spy)
import Components.Window as Window
import URI.Extra.QueryPairs as QP
import ChatBot.Models (ChannelName(..))
import Elmish (Transition(..), pureUpdate)

main :: Effect Unit
main = do
  mStream <- getStreamFromParams
  case mStream of
    Just s -> do
      let c = ChannelName { _unChannelName : "#" <> s }
      commands <- go "Commands" $ Commands.def c
      quotes   <- go "Quotes" $ Quotes.def c
      Elmish.boot { domElementId: "app", def: Tabs.tabs [commands, quotes] }
    Nothing -> Elmish.boot { domElementId: "app", def: emptyDef }

  where
  go :: forall msg state. String -> ComponentDef OpM msg state -> Effect { title :: String, view :: ReactElement }
  go title def = do
    render <- Elmish.construct (Elmish.nat runOpM def)
    pure { title: title, view: render onError }

  onError :: DispatchMsgFn Unit
  onError = dispatchMsgFn log (const $ pure unit)

  emptyDef = { init: Transition {} [], update: \_ _ -> pureUpdate {}, view: \_ _ -> empty }

-- TODO: this _could_ be an (Array String) if i feel like it. not sure yet.
getStreamFromParams :: Effect (Maybe String)
getStreamFromParams = getStreamFromParams' <$> Window.getSearchParams
  where
  getStreamFromParams' :: forall e. Either e (QP.QueryPairs String String) -> Maybe String
  getStreamFromParams' (Left e) = Nothing
  getStreamFromParams' (Right (QP.QueryPairs ps)) = head $ mapMaybe f ps
    where
    f (Tuple k (Just v)) | k == "stream" = Just v
    f _ = Nothing
