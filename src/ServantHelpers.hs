
module ServantHelpers (
    module Servant
  , module Servant.API.Generic
  , module Servant.Server.Generic
  , Compose
  , guard401
  , maybeOr401
  , maybeOr404
  , maybeOr500
  , adminOr401
  , eitherOr401
  , callerIsUserOr401
  , callerIsUserOrIsAdminElse401
  , unexpected
  ) where

import           Auth.DatabaseModels    (DbUserId)
import           Auth.Models            (User (..))
import           Control.Monad.Except   (MonadError)
import           Data.Text              (Text)
import           Database.Esqueleto     (fromSqlKey)
import           Error                  (AppError (..), AuthError (..), ChatBotError,
                                         ChatBotError' (..))
import           Protolude
import           Servant
import           Servant.API.Generic
import qualified Servant.API.Generic    as S
import           Servant.Server.Generic (AsServerT, genericServerT)

---
--- servant generic helpers
---

-- This alias encapsulates this frequently repeated pattern that is used for
-- nesting Servant.API.Generic-powered APIs.
type Compose api = S.ToServant api S.AsApi

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
eitherOr401 :: MonadError (AppError e) m => Either e' a -> (a -> m b) -> m b
eitherOr401 = eitherOrErr (AppAuthError NoAuthError)

-- |
maybeOr500 :: MonadError (AppError e) m => Text -> Maybe a -> (a -> m b) -> m b
maybeOr500 msg = maybeOrErr (AppUnexpectedError msg)

-- |
maybeOrErr :: MonadError (AppError e) m => AppError e -> Maybe a -> (a -> m b) -> m b
maybeOrErr err = flip $ maybe (throwError err)

-- |
eitherOrErr :: MonadError (AppError e) m => AppError e -> Either e' a -> (a -> m b) -> m b
eitherOrErr err e f = either (const $ throwError err) f e

-- |
adminOr401 :: MonadError (AppError e) m => User -> m a -> m a
adminOr401 u m = orNoAuth m $ userAdmin u

-- |
callerIsUserOr401 :: MonadError (AppError e) m => User -> DbUserId -> m a -> m a
callerIsUserOr401 caller uid m = orNoAuth m $ userId caller == fromSqlKey uid
--
-- |
callerIsUserOrIsAdminElse401 :: MonadError (AppError e) m => User -> DbUserId -> m a -> m a
callerIsUserOrIsAdminElse401 caller uid m =
  orNoAuth m $ userAdmin caller || userId caller == fromSqlKey uid

-- |
orNoAuth :: MonadError (AppError e) m => m a -> Bool -> m a
orNoAuth m b = if b then m else throwError (AppAuthError NoAuthError)
