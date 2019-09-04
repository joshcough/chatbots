module Components.Tabs
    ( Item
    , State
    , tabs
    ) where

import Prelude

import Data.Array (head, (!!), (:))
import Data.Maybe (Maybe, maybe)
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Aff)
import Elmish (ComponentDef, ReactComponent, ReactElement, Transition(..), JsCallback0, createElement', handle, pureUpdate)
import Elmish.React.DOM (empty)

type Item = { title :: String, view :: ReactElement }
type State = {}
type Message = Unit

tabs :: Array Item -> ComponentDef Aff Message State
tabs items =
    { init: Transition {} []
    , update: \s _ -> pureUpdate {}
    , view: \s dispatch -> createElement' view_
        { items: itemView dispatch `map` items
        , defaultActiveKey: toNullable $ _.title <$> head items
        }
    }
    where
        itemView dispatch i = { title: i.title, inner: i.view }

foreign import view_ :: ReactComponent
    { items :: Array { title :: String, inner :: ReactElement }
    , defaultActiveKey :: Nullable String
    }
