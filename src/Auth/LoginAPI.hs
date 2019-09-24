
module Auth.LoginAPI where

import Protolude

import Auth.Models (Login(..), User(..))
import qualified Auth.UserStorage as Db
import Config (HasConfig, configCookies, configJWT)
import Control.Lens (view)
import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Crypto.BCrypt (validatePassword)
import Data.String.Conversions (cs)
import Data.Text.Encoding (encodeUtf8)
import Error (AppError)
import Servant.Auth.Server
import ServantHelpers
import Types (AppT)

type SetCookieHeader  = Header "Set-Cookie" SetCookie
type SetCookieHeaders = '[SetCookieHeader, SetCookieHeader]

---
--- Login API/Server
---

type LoginAPI = "login" :> Compose LoginServer

data LoginServer r = LoginServer {
    loginServerLogin :: r :- "header" :> ReqBody '[JSON] Login :> Post '[JSON] (Headers SetCookieHeaders User)
  , loginServerToken :: r :- "token" :> ReqBody '[JSON] Login :> Post '[JSON] Text
  } deriving Generic

loginServer :: MonadIO m => ServerT LoginAPI (AppT m)
loginServer = toServant $ LoginServer login loginToken

{-
 - Here is the login handler. We do the following:
 - A) look up the user in the database by email addr, and throw 404 if not found
 - B) Check to see if they entered a valid password, and throw a 401 if not
 - C) Return the jwt token in the header.
 -}
login :: MonadIO m => Login -> AppT m (Headers SetCookieHeaders User)
login l = withLoggedInUser l applyCookies
    where
    applyCookies usr cookieSettings jwtSettings = do
        mApplyCookies  <- liftIO $ acceptLogin cookieSettings jwtSettings usr
        maybeOr401 mApplyCookies (\app -> return . app $ usr)

{-
  Like the normal login, but, return the jwt token as text instead of storing it in the headers.
-}
loginToken :: MonadIO m => Login -> AppT m Text
loginToken l = withLoggedInUser l $ \user cookieSettings jwtSettings -> do
    ejwt <- liftIO $ makeJWT user jwtSettings (cookieExpires cookieSettings)
    eitherOr401 ejwt (pure . cs)

withLoggedInUser :: (MonadReader s m, Db.UserDb m, MonadError (AppError e) m, HasConfig s) =>
    Login -> (User -> CookieSettings -> JWTSettings -> m b) -> m b
withLoggedInUser (Login e pw) f = do
    mu <- Db.getUserByEmail e
    maybeOr401 mu $ \(user, hashedPw) -> guard401 (validate hashedPw) $ do
        cookieSettings <- view configCookies
        jwtSettings <- view configJWT
        f user cookieSettings jwtSettings
    where
    validate hashedPw = validatePassword (encodeUtf8 hashedPw) (encodeUtf8 pw)
