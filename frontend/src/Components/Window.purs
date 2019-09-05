module Components.Window where

import Prelude
import Effect (Effect)
import Data.Either (Either)
import Data.String (drop)
import URI.Common (URIPartParseError)
import URI.Query as Query
import URI.Extra.QueryPairs as QP

foreign import href_ :: Effect String
foreign import search_ :: Effect String

getSearchParams ::  Effect (Either URIPartParseError (QP.QueryPairs String String))
getSearchParams = do
  s <- Query.fromString <<< drop 1 <$> search_
  pure $ QP.parse (\k -> pure $ QP.keyToString k) (\v -> pure $ QP.valueToString v) s
