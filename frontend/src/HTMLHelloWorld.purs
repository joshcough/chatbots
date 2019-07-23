module HTMLHelloWorld
    ( main'
    ) where

import Prelude
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (either)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (Aff, error)
import Elmish.HTML as R
import Elmish (ReactElement, boot, ComponentDef, nat, DispatchMsgFn, JsCallback0, ReactComponent, Transition(..), createElement', handle, pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, noData)

-- import JSX.Web.Core.Atoms.Layout.Grid (col, row)
-- import JSX.Web.Core.Atoms.Layout.Grid as Col

main' :: Effect Unit
main' = boot { domElementId: "app" , def: nat runOpM def }

data Message = Inc | Dec | GotInt Int

type State = { count :: Int }

type OpM' c = ReaderT c (ExceptT HttpException Aff)
type OpM = OpM' Unit

runOpM :: forall a . OpM a -> Aff a
runOpM = runOpM' unit

runOpM' :: forall context a
     . context
    -> OpM' context a
    -> Aff a
runOpM' context f = do
  res <- runExceptT (runReaderT f context)
  either (throwError <<< error <<< show) pure res

def :: ComponentDef OpM Message State
def =
  { init: Transition { count: 0 } []
  , update
  , view
  }
  where
    update s Inc = Transition s [ GotInt <$> getRemoteInt ]
    update s Dec = pureUpdate s { count = s.count-1 }
    update s (GotInt i) = pureUpdate s { count = s.count+i }
    view s dispatch = helloWorld s dispatch

getRemoteInt :: forall m . MonadAff m => MonadError HttpException m => m Int
getRemoteInt = httpJSON $ buildReq GET "http://localhost:8081/random_int" noData

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
