module Api (app) where

import Protolude

import           Control.Monad.Except     ( MonadIO, liftIO, throwError )
import           ServantHelpers
import           Servant.Auth.Server      hiding (throwAll)
import           System.Random            (randomIO)

import           Auth.Models              ( MiniUser )
import           Error                    ( AppError(..), AuthError(..), toServantErr, throwAll )
import           Types                    ( App, AppT, Config (..), runAppT )

type ChatBotAPI' auths = (Auth auths MiniUser :> Protected) :<|> Unprotected
type ChatBotAPI        = Unprotected -- ChatBotAPI' '[Cookie, JWT]

type IntAPI' = Compose IntServer'

newtype IntServer' r = IntServer' {
    intServerHello :: r :- Get '[JSON] Int
 } deriving Generic

intServer' :: MonadIO m => ServerT IntAPI' (AppT m)
intServer' = toServant $ IntServer' {
    intServerHello = liftIO $ (\b -> if b then 1 else 2) <$> randomIO
}

type EmptyAPI' = Compose EmptyServer'

newtype EmptyServer' r = EmptyServer' {
    emptyServerHello :: r :- Get '[JSON] ()
 } deriving Generic


emptyServer' :: MonadIO m => ServerT EmptyAPI' (AppT m)
emptyServer' = toServant $ EmptyServer' {
    emptyServerHello = return ()
}

type Protected = Compose ProtectedServer

-- | Lives behind authorization. Only logged in users can visit these pages.
newtype ProtectedServer route = ProtectedServer {
    protectedServerEmptyApi :: route :- EmptyAPI'
  } deriving Generic

type Unprotected = Compose UnprotectedServer

-- | Not protected by any authorization. Anyone can visit these pages.
newtype UnprotectedServer route = UnprotectedServer {
    unprotectedServerEmptyApi :: route :- "random_int" :> IntAPI'
  } deriving Generic

-- |
unprotectedServer :: MonadIO m => ServerT Unprotected (AppT m)
unprotectedServer = intServer'

-- |
protectedServer :: MonadIO m => AuthResult MiniUser -> ServerT Protected (AppT m)
protectedServer (Authenticated _) = emptyServer'
protectedServer _ = throwAll (AppAuthError NoAuthError)

-- | The main application for the Proverlays backend.
app :: Config -> Application
app cfg = serveWithContext
            (Proxy :: Proxy (ChatBotAPI :<|> Raw))
            (_configCookies cfg :. _configJWT cfg :. EmptyContext)
            (mainServer :<|> serveDirectoryFileServer "frontend")
    where
    convertApp :: Config -> App a -> Handler a
    convertApp cfg' appt = Handler $
        liftIO (runAppT appt cfg') >>= either (throwError . toServantErr) return

    mainServer :: Server ChatBotAPI
    mainServer = hoistServerWithContext
        (Proxy :: Proxy ChatBotAPI)
        (Proxy :: Proxy '[CookieSettings, JWTSettings])
        (convertApp cfg)
        ({-protectedServer :<|> -}unprotectedServer)

