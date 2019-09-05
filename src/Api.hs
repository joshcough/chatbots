module Api (app) where

import Protolude

import Control.Monad.Except (MonadIO, liftIO, throwError)
import Servant.Auth.Server hiding (throwAll)
import ServantHelpers
import System.Random (randomIO)

import Auth.Models (MiniUser)
import ChatBot.ChatBotAPI (ChatBotAPI, chatBotServer)
import Error (AppError(..), AuthError(..), throwAll, toServantErr)
import Types (App, AppT, Config(..), runAppT)

type TopLevelAPI' auths = (Auth auths MiniUser :> Protected) :<|> Unprotected
type TopLevelAPI        = TopLevelAPI' '[Cookie, JWT]

type Protected = Compose ProtectedServer

-- | Lives behind authorization. Only logged in users can visit these pages.
newtype ProtectedServer route = ProtectedServer {
    protectedRandomInt :: route :- "protected_random_int" :> Get '[JSON] Int
  } deriving Generic

protectedServer :: MonadIO m => MiniUser -> ServerT Protected (AppT m)
protectedServer _ = toServant $ ProtectedServer {
    protectedRandomInt = liftIO $ (\b -> if b then 1 else 2) <$> randomIO
}

type Unprotected = Compose UnprotectedServer

-- | Not protected by any authorization. Anyone can visit these pages.
data UnprotectedServer route = UnprotectedServer {
    unprotectedRandomInt :: route :- "random_int" :> Get '[JSON] Int
  , unrotectedChatBotApi :: route :- ChatBotAPI
  } deriving Generic

-- |
unprotectedServer :: (MonadIO m) => ServerT Unprotected (AppT m)
unprotectedServer = toServant $ UnprotectedServer {..}
    where
    unprotectedRandomInt = liftIO $ (\b -> if b then 1 else 2) <$> randomIO
    unrotectedChatBotApi = chatBotServer


-- | The main application for the Proverlays backend.
app :: Config -> Application
app cfg = serveWithContext
            (Proxy :: Proxy (TopLevelAPI :<|> Raw))-- :<|> Raw))
            (_configCookies cfg :. _configJWT cfg :. EmptyContext)
            (mainServer :<|> serveDirectoryFileServer "frontend") -- :<|> Tagged app2)
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
