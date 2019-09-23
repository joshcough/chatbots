module Api (app) where

import Protolude

import Auth.Models (User)
import ChatBot.Server.ChatBotAPI
  ( ProtectedChatBotAPI
  , UnprotectedChatBotAPI
  , chatBotServerProtected
  , chatBotServerUnprotected
  )
import Control.Monad.Except (MonadIO, liftIO, throwError)
import Error (AppError(..), AuthError(..), throwAll, toServantErr)
import Servant.Auth.Server hiding (throwAll)
import ServantHelpers
import Types (App, AppT, Config(..), runAppT)

type TopLevelAPI' auths = (Auth auths User :> Protected) :<|> Unprotected
type TopLevelAPI        = TopLevelAPI' '[Cookie, JWT]

type Protected = Compose ProtectedServer

-- | Lives behind authorization. Only logged in users can visit these pages.
newtype ProtectedServer route = ProtectedServer {
    protectedChatBotApi :: route :- ProtectedChatBotAPI
  } deriving Generic

protectedServer :: MonadIO m => User -> ServerT Protected (AppT m)
protectedServer u = toServant $ ProtectedServer {
    protectedChatBotApi = chatBotServerProtected u
}

type Unprotected = Compose UnprotectedServer

-- | Not protected by any authorization. Anyone can visit these pages.
newtype UnprotectedServer route = UnprotectedServer {
    unprotectedChatBotApi :: route :- UnprotectedChatBotAPI
  } deriving Generic

-- |
unprotectedServer :: (MonadIO m) => ServerT Unprotected (AppT m)
unprotectedServer = toServant $ UnprotectedServer {..}
    where
    unprotectedChatBotApi = chatBotServerUnprotected

-- | The main application for the Proverlays backend.
app :: Config -> Application
app cfg = serveWithContext
            (Proxy :: Proxy (TopLevelAPI :<|> Raw))
            (_configCookies cfg :. _configJWT cfg :. EmptyContext)
            (mainServer :<|> serveDirectoryFileServer "frontend")
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
