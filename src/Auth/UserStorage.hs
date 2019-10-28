
module Auth.UserStorage (
    UserDb(..)
  , encryptPassword
  ) where

import           Auth.DatabaseModels         (DbUser (..), DbUserId)
import qualified Auth.DatabaseModels         as Db
import           Auth.Models                 (CreateUser (..), User (..))
import           Config                      (HasConfig)
import           Control.Monad.Except        (MonadIO, liftIO)
import           Crypto.BCrypt               (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Database.Esqueleto          (Entity (..), SqlPersistT, fromSqlKey, getEntity,
                                              insert, replace, selectFirst)
import qualified Database.Persist.Postgresql as P
import           Protolude                   hiding (replace)
import           Types                       (AppT', runDb)

class Monad m => UserDb m where
    getUserById :: DbUserId -> m (Maybe User)
    getUserByUsername :: Text -> m (Maybe User)
    getUserByEmail :: Text -> m (Maybe (User, Text))
    deleteUserById :: DbUserId -> m ()
    createUser :: CreateUser -> m (Maybe DbUserId)
    -- TODO: this one is terrible.
    updateUserIfExists :: DbUserId -> User -> m ()

instance (HasConfig c, MonadIO m) => UserDb (AppT' e m c) where
  getUserById = runDb . getUserById
  getUserByUsername = runDb . getUserByUsername
  getUserByEmail = runDb . getUserByEmail
  deleteUserById = runDb . deleteUserById
  createUser = runDb . createUser
  updateUserIfExists uid = runDb . updateUserIfExists uid

instance MonadIO m => UserDb (SqlPersistT m) where
  getUserById = fmap (fmap entityToUser) . getEntity

  getUserByUsername username = fmap entityToUser <$> selectFirst [Db.DbUserName P.==. username] []

  getUserByEmail email = fmap f <$> selectFirst [Db.DbUserEmail P.==. email] []
    where f e@(Entity _ dbUser) = (entityToUser e, dbUserHashedPassword dbUser)

  deleteUserById = P.deleteCascade

  createUser (CreateUser name email pw) = liftIO (encryptPassword pw) >>= \case
    Nothing -> return Nothing
    Just pw' -> fmap Just $ insert $ DbUser name email (decodeUtf8 pw') False

  updateUserIfExists uid (User _ name email _) = getEntity uid >>= \case
    Just (Entity k v) -> replace k $ DbUser name email (dbUserHashedPassword v) (dbUserAdmin v)
    Nothing -> return ()

-- |
entityToUser :: Entity DbUser -> User
entityToUser (Entity k DbUser {..}) = User (fromSqlKey k) dbUserName dbUserEmail dbUserAdmin

-- |
encryptPassword :: Text -> IO (Maybe ByteString)
encryptPassword = hashPasswordUsingPolicy slowerBcryptHashingPolicy . encodeUtf8
