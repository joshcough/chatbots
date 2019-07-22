module HTMLHelloWorld
    ( main'
    ) where

import Prelude
import Effect (Effect)
import Elmish (ReactElement, boot)
import Elmish.HTML as R
import Elmish (ComponentDef, DispatchMsgFn, JsCallback0, ReactComponent, Transition(..), createElement', handle, pureUpdate)

-- import JSX.Web.Core.Atoms.Layout.Grid (col, row)
-- import JSX.Web.Core.Atoms.Layout.Grid as Col

main' :: Effect Unit
main' = boot { domElementId: "app" , def: def }

data Message = Inc | Dec

type State = { count :: Int }

def :: forall m. ComponentDef m Message State
def =
  { init: Transition { count: 0 } []
  , update
  , view
  }
  where
    update s Inc = pureUpdate s { count = s.count+1 }
    update s Dec = pureUpdate s { count = s.count-1 }
    view s dispatch = helloWorld s dispatch

foreign import view_ :: ReactComponent
  { count :: Int
  , onInc :: JsCallback0
  , onDec :: JsCallback0
  }

helloWorld :: State -> DispatchMsgFn Message -> ReactElement
helloWorld s dispatch = R.article { className: "container" }
        [ R.h1 {} "PureScript Elmish: HTML Hello World"
        , R.p {className:"text-right"} "Lorem ipsum dolor sit amet, consectetur adipiscing elit molestie."
        , R.img { src: "http://placekitten.com/780/540", width: "780", height: "540" }
        , R.p {} "Pellentesque libero mi, feugiat at ligula et, blandit dignissim non."
        -- , row {} [ col { col: Col.span5 } "test??" ]
        , createElement' view_
                { count: s.count
                , onInc: handle dispatch Inc
                , onDec: handle dispatch Dec
                }
        ]
