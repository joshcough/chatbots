module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Elmish as Elmish
import Elmish.Component (ComponentDef)
import Elmish.Dispatch (DispatchMsgFn, dispatchMsgFn)
import Elmish.React (ReactElement)
import Components.Tabs as Tabs
import Commands as Commands
import Quotes as Quotes
import Types (OpM, runOpM)
import Debug.Trace (spy)
import Components.Window as Window

main :: Effect Unit
main = do
  params <- Window.getSearchParams
  pure $ spy (show params) unit

  commands <- go "Commands" Commands.def
  quotes   <- go "Quotes"  Quotes.def
  Elmish.boot { domElementId: "app", def: Tabs.tabs [commands, quotes] }

  where
  go :: forall msg state. String -> ComponentDef OpM msg state -> Effect { title :: String, view :: ReactElement }
  go title def = do
    render <- Elmish.construct (Elmish.nat runOpM def)
    pure { title: title, view: render onError }

  onError :: DispatchMsgFn Unit
  onError = dispatchMsgFn log (const $ pure unit)
