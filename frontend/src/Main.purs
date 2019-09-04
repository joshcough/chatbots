module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Elmish as Elmish
import Elmish.Component (ComponentDef)
import Elmish.Dispatch (dispatchMsgFn)
import Elmish.React (ReactElement)
import Frame as Frame
import Commands as Commands
import Quotes as Quotes
import Types (OpM, runOpM)

main :: Effect Unit
main = do
    commands <- go "Comands" Commands.def
    quotes   <- go "Quotes"  Quotes.def
    Elmish.boot { domElementId: "app", def: Frame.frame [commands, quotes] }

go :: forall msg state. String -> ComponentDef OpM msg state -> Effect { title :: String, view :: ReactElement }
go title def = do
  render <- Elmish.construct (Elmish.nat runOpM def)
  pure { title: title, view: render onError }
  where onError = dispatchMsgFn log (const $ pure unit)