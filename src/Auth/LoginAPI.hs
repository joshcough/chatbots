
module Auth.LoginAPI where

import Protolude

import Auth.Models (Login(..), User(..))
import qualified Auth.UserStorage as Db
import Config (Config, configCookies, configJWT)
import Control.Lens (view)
import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Crypto.BCrypt (validatePassword)
import Data.Text.Encoding (encodeUtf8)
import Error (ChatBotError)
import Servant.Auth.Server
import ServantHelpers
import Types (AppT)

type SetCookieHeader  = Header "Set-Cookie" SetCookie
type SetCookieHeaders = '[SetCookieHeader, SetCookieHeader]

---
--- Login API/Server
---

type LoginAPI = "login" :> Compose LoginServer

newtype LoginServer route = LoginServer {
    loginServerLogin :: route :- ReqBody '[JSON] Login :> Post '[JSON] (Headers SetCookieHeaders User)
  } deriving Generic

loginServer :: MonadIO m => ServerT LoginAPI (AppT m)
loginServer = toServant $ LoginServer login

{-
 - Here is the login handler. We do the following:
 - A) look up the user in the database by email addr, and throw 404 if not found
 - B) Check to see if they entered a valid password, and throw a 401 if not
 - C) Return the jwt token in the header.
 -}
login :: MonadIO m => Login -> AppT m (Headers SetCookieHeaders User)
login (Login e pw) = do
    mu <- Db.getUserByEmail e
    maybeOr401 mu $ \(user, hashedPw) -> guard401 (validate hashedPw) (applyCookies user)
    where
    validate hashedPw = validatePassword (encodeUtf8 hashedPw) (encodeUtf8 pw)

-- |
applyCookies :: (MonadError ChatBotError m, MonadIO m, MonadReader Config m) =>
    User -> m (Headers SetCookieHeaders User)
applyCookies usr = do
    cookieSettings <- view configCookies
    jwtSettings    <- view configJWT
    mApplyCookies  <- liftIO $ acceptLogin cookieSettings jwtSettings usr
    maybeOr401 mApplyCookies (\app -> return . app $ usr)
