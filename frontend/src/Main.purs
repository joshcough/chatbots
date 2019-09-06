module Main where

import Prelude

import Data.Array (mapMaybe, head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
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
import Network.Endpoints (getStreams)

main :: Effect Unit
main = launchAff_ $ do
  streams <- runOpM getStreams
  liftEffect $ do
    mStream <- getStreamFromUrlParams
    let c = ChannelName { _unChannelName : fromMaybe "#daut" mStream }
    Elmish.boot { domElementId: "app", def: Elmish.nat runOpM $ Quotes.def streams c }

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

-- TODO: this _could_ be an (Array String) if i feel like it. not sure yet.
getStreamFromUrlParams :: Effect (Maybe String)
getStreamFromUrlParams = getStreamFromParams' <$> Window.getSearchParams
  where
  getStreamFromParams' :: forall e. Either e (QP.QueryPairs String String) -> Maybe String
  getStreamFromParams' (Left e) = Nothing
  getStreamFromParams' (Right (QP.QueryPairs ps)) = head $ mapMaybe f ps
    where
    f (Tuple k (Just v)) | k == "stream" = Just $ "#" <> v
    f _ = Nothing
