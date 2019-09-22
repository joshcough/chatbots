module Types
    ( OpM, OpM', Config, runOpM
    ) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (either)
import Effect.Aff (Aff, error)
import Network.HTTP (HttpException)
import Data.Maybe (Maybe)

type Config = { hostname :: Maybe String }

type OpM' c = ReaderT c (ExceptT HttpException Aff)
type OpM = OpM' Config

runOpM :: forall context a . context -> OpM' context a -> Aff a
runOpM context f = do
  res <- runExceptT (runReaderT f context)
  either (throwError <<< error <<< show) pure res
