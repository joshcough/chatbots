
module ServantHelpers (
    module Servant
  , module Servant.API.Generic
  , module Servant.Server.Generic
  , Compose
  , toServant
  , guard401
  , maybeOr401
  , maybeOr404
  , maybeOr500
--  , adminOr401
--  , callerIsUserOr401
--  , callerIsUserOrIsAdminElse401
--  , callerIsOnTeamOrIsAdminElse401
--  , userIsOnTeamElse401
  , unexpected
  ) where

import Protolude

import           Control.Monad.Except        (MonadError)
--import           Database.Persist.Postgresql (fromSqlKey)
import           Data.Text                   (Text)
import           Servant
import           Servant.API.Generic         hiding (toServant)
import qualified Servant.API.Generic as S
import           Servant.Server.Generic      (AsServerT, genericServerT)
import           Error                       (AppError(..), AuthError(..), ChatBotError, ChatBotError'(..))

---
--- servant generic helpers
---

-- This alias encapsulates this frequently repeated pattern that is used for
-- nesting Servant.API.Generic-powered APIs.
type Compose api = S.ToServant api S.AsApi

-- This function used to be necessary before servant-generic was merged into the
-- main servant package, it provided some better type inference for use sites.
-- Currently it's no longer necessary - both its type and its implementation are
-- identical to the library-provided `genericServerT`, but it was left here
-- anyway in order to avoid modifying all use sites. We will do that later.
toServant :: forall m api
     . GenericServant api (AsServerT m)
    => api (AsServerT m)
    -> S.ToServant api (AsServerT m)
toServant = genericServerT

-- |
unexpected :: MonadError ChatBotError m => Text -> m a
unexpected = throwError . AppAppError . ChatBotMiscError

-- |
guard401 :: MonadError (AppError e) m => Bool -> m a -> m a
guard401 b m = if b then m else throwError $ AppNotFoundError ""

-- |
maybeOr404 :: MonadError (AppError e) m => Maybe a -> (a -> m b) -> m b
maybeOr404 = maybeOrErr $ AppNotFoundError ""

-- |
maybeOr401 :: MonadError (AppError e) m => Maybe a -> (a -> m b) -> m b
maybeOr401 = maybeOrErr (AppAuthError NoAuthError)

-- |
maybeOr500 :: MonadError (AppError e) m => Text -> Maybe a -> (a -> m b) -> m b
maybeOr500 msg = maybeOrErr (AppUnexpectedError msg)

-- |
maybeOrErr :: MonadError (AppError e) m => AppError e -> Maybe a -> (a -> m b) -> m b
maybeOrErr err = flip $ maybe (throwError err)

---- |
--adminOr401 :: UserData u => MonadError (AppError e) m => u -> m a -> m a
--adminOr401 u m = orNoAuth m $ isAdmin u
--
---- |
--callerIsUserOr401 :: UserData u => MonadError (AppError e) m => u -> DbUserId -> m a -> m a
--callerIsUserOr401 caller uid m = orNoAuth m $ getUserId caller == fromSqlKey uid
--
---- |
--callerIsUserOrIsAdminElse401 :: UserData u => MonadError (AppError e) m => u -> DbUserId -> m a -> m a
--callerIsUserOrIsAdminElse401 caller uid m = orNoAuth m $ isAdmin caller || getUserId caller == fromSqlKey uid
--
---- |
--callerIsOnTeamOrIsAdminElse401 :: UserData u => MonadError (AppError e) m => u -> DbTeamId -> m a -> m a
--callerIsOnTeamOrIsAdminElse401 caller tid m = orNoAuth m $ isAdmin caller || getTeamId caller == fromSqlKey tid
--
---- |
--userIsOnTeamElse401 :: UserData u => MonadError (AppError e) m => u -> DbTeamId -> m a -> m a
--userIsOnTeamElse401 user tid m = orNoAuth m $ getTeamId user == fromSqlKey tid
--
---- |
--orNoAuth :: MonadError (AppError e) m => m a -> Bool -> m a
--orNoAuth m b = if b then m else throwError (AppAuthError NoAuthError)