module Api (app) where

import Protolude

import Auth.LoginAPI (LoginAPI, loginServer)
import Auth.Models (User)
import Auth.UserAPI (UserAPI, userServer)
import ChatBot.Server.ChatBotAPI
  ( ProtectedChatBotAPI
  , UnprotectedChatBotAPI
  , chatBotServerProtected
  , chatBotServerUnprotected
  )
import Control.Monad.Except (MonadIO, liftIO, throwError)
import Error (AppError(..), AuthError(..), throwAll, toServantErr)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static (defaultFileServerSettings)
import Servant.Auth.Server hiding (throwAll)
import ServantHelpers
import System.FilePath (addTrailingPathSeparator)
import Types (App, AppT, Config(..), runAppT)
import WaiAppStatic.Types (StaticSettings(..))

type TopLevelAPI' auths = (Auth auths User :> Protected) :<|> Unprotected
type TopLevelAPI        = TopLevelAPI' '[Cookie, JWT]

type Protected = Compose ProtectedServer

-- | Lives behind authorization. Only logged in users can visit these pages.
data ProtectedServer route = ProtectedServer {
    protectedChatBotApi :: route :- ProtectedChatBotAPI
  , protectedUserApi :: route :- UserAPI
  } deriving Generic

protectedServer :: MonadIO m => User -> ServerT Protected (AppT m)
protectedServer u = toServant $ ProtectedServer {
    protectedChatBotApi = chatBotServerProtected u
  , protectedUserApi = userServer u
}

type Unprotected = Compose UnprotectedServer

-- | Not protected by any authorization. Anyone can visit these pages.
data UnprotectedServer route = UnprotectedServer {
    unprotectedLoginApi :: route :- LoginAPI
  , unprotectedChatBotApi :: route :- UnprotectedChatBotAPI
  } deriving Generic

-- |
unprotectedServer :: (MonadIO m) => ServerT Unprotected (AppT m)
unprotectedServer = toServant $ UnprotectedServer {..}
    where
    unprotectedLoginApi = loginServer
    unprotectedChatBotApi = chatBotServerUnprotected

-- | The main application for the Proverlays backend.
app :: Config -> Application
app cfg = serveWithContext
            (Proxy :: Proxy (TopLevelAPI :<|> Raw))
            (_configCookies cfg :. _configJWT cfg :. EmptyContext)
            (mainServer :<|> serveDirectoryFileServer' "frontend")
    where
    convertApp :: Config -> App a -> Handler a
    convertApp cfg' appt = Handler $
        liftIO (runAppT appt cfg') >>= either (throwError . toServantErr) return

    protectedServer' (Authenticated u) = protectedServer u
    protectedServer' _ = throwAll (AppAuthError NoAuthError)

    mainServer :: Server TopLevelAPI
    mainServer = hoistServerWithContext
        (Proxy :: Proxy TopLevelAPI)
        (Proxy :: Proxy '[CookieSettings, JWTSettings])
        (convertApp cfg)
        (protectedServer' :<|> unprotectedServer)

serveDirectoryFileServer' :: FilePath -> ServerT Raw m
serveDirectoryFileServer' p = serveDirectoryWith settings
  where
  settings :: StaticSettings
  settings = (defaultFileServerSettings $ addTrailingPathSeparator p) {
    ss404Handler = Just indexApp
  }

  indexApp :: Application
  indexApp _ respond = respond index

  index :: Response
  index = responseFile status200 [("Content-Type", "text/html")] "frontend/index.html" Nothing
