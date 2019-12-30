
module Auth.UserAPI where

import           Auth.DatabaseModels  (DbUserId)
import           Auth.Models          (CreateUser (..), User (..))
import qualified Auth.UserStorage     as Db
import           Control.Monad.Except (MonadIO)
import           Logging              (logDebug, (.=))
import           Protolude
import           ServantHelpers
import           Types                (AppT, ChatBotM)

---
--- User API/Server
---

type UserM m = (ChatBotM m, Db.UserDb m)

type UserAPI = "users" :> Compose UserServer

data UserServer r = UserServer {
    userServerGetUserById :: r :- Capture "id" DbUserId :> Get '[JSON] User
  , userServerDeleteUser  :: r :- Capture "id" DbUserId :> Delete '[JSON] ()
  , userServerCreateUser  :: r :- ReqBody '[JSON] CreateUser :> Post '[JSON] DbUserId
  , userServerUpdateUser  :: r :- Capture "id" DbUserId :> ReqBody '[JSON] User :> Put '[JSON] ()
  } deriving Generic

-- | The server that runs the UserAPI
userServer :: (MonadIO m) => User -> ServerT UserAPI (AppT m)
userServer caller = genericServerT $ UserServer { .. }
 where
  userServerGetUserById = getUserById caller
  userServerDeleteUser = deleteUser caller
  userServerCreateUser = createUser caller
  userServerUpdateUser = updateUser caller

-- | Returns a user by name or throws a 404 error.
getUserById :: UserM m => User -> DbUserId -> m User
getUserById caller uid = do
  $(logDebug) "getUserById" ["uid" .= uid]
  callerIsUserOrIsAdminElse401 caller uid $ withUserOr404 uid return

-- | Creates a user in the database.
deleteUser :: UserM m => User -> DbUserId -> m ()
deleteUser caller uid = do
  $(logDebug) "deleteUser" ["uid" .= uid]
  adminOr401 caller $ withUserOr404 uid (const $ Db.deleteUserById uid)

-- | Creates a user in the database.
createUser :: UserM m => User -> CreateUser -> m DbUserId
createUser caller c = do
  $(logDebug) "createUser" []
  adminOr401 caller $ Db.createUser c >>= flip (maybeOr500 "Couldn't create user.") return

-- | Update a user in the database.
updateUser :: UserM m => User -> DbUserId -> User -> m ()
updateUser caller uid u = do
  $(logDebug) "updateUser" ["uid" .= uid, "user" .= u]
  callerIsUserOrIsAdminElse401 caller uid $ withUserOr404 uid . const $ Db.updateUserIfExists uid u

-- | Look up a user by id. If it exist, run an operation on it. If not, throw a 404.
withUserOr404 :: UserM m => DbUserId -> (User -> m b) -> m b
withUserOr404 uid m = Db.getUserById uid >>= flip maybeOr404 m
