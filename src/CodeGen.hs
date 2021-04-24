{-# OPTIONS_GHC -fno-warn-orphans #-}

module CodeGen (genTS, printTS, genPS) where

import           Auth.Models
import           ChatBot.Models
import           Data.Aeson                         (defaultOptions)
import           Data.Aeson.TypeScript.TH
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import           Language.PureScript.Bridge
--import           Language.PureScript.Bridge.SumType (equal)
import           Protolude

srcDir :: FilePath
srcDir = "frontend/src"

----------------
-- Purescript --
----------------

genPS :: IO ()
genPS = writePSTypes srcDir (buildBridge defaultBridge) psDecls

-- TODO: there are more types in Models that maybe should be added here
psDecls :: [SumType 'Haskell]
psDecls = [
  -- Auth.Models
    e (Proxy :: Proxy CreateUser)
  , e (Proxy :: Proxy Login)
  , e (Proxy :: Proxy Token)
  , e (Proxy :: Proxy User)
  , e (Proxy :: Proxy UserToken)
  -- ChatBot.Models
  , o (Proxy :: Proxy ChannelName)
  , e (Proxy :: Proxy ChatUser)
  , o (Proxy :: Proxy ChatUserName)
  , o (Proxy :: Proxy Command)
  , o (Proxy :: Proxy Quote)
  , o (Proxy :: Proxy Stream)
  ]
 where
  o p = order p (mkSumType p)
  e p = equal p (mkSumType p)

----------------
-- Typescript --
----------------

instance TypeScript Int64 where
  getTypeScriptType _ = "number"

-- Auth.Models
$(deriveTypeScript defaultOptions ''Login)
$(deriveTypeScript defaultOptions ''User)
$(deriveTypeScript defaultOptions ''CreateUser)
$(deriveTypeScript defaultOptions ''Token)
$(deriveTypeScript defaultOptions ''UserToken)

-- ChatBot.Models
$(deriveTypeScript defaultOptions ''ChannelName)
$(deriveTypeScript defaultOptions ''ChatUser)
$(deriveTypeScript defaultOptions ''ChatUserName)
$(deriveTypeScript defaultOptions ''Command)
$(deriveTypeScript defaultOptions ''Quote)
$(deriveTypeScript defaultOptions ''Stream)

genTS :: IO ()
genTS = T.writeFile (srcDir <> "/types.ts") ts

printTS :: IO ()
printTS = T.putStrLn ts

ts :: Text
ts = T.pack . formatTSDeclarations $ tsDecls

tsDecls :: [TSDeclaration]
tsDecls = join $ [
  -- Auth.Models
    f (p @UserToken)
  , f (p @Token)
  , f (p @CreateUser)
  , f (p @User)
  , f (p @Login)
  -- ChatBot.Models
  , f (p @ChannelName)
  , f (p @ChatUser)
  , f (p @ChatUserName)
  , f (p @Command)
  , f (p @Quote)
  , f (p @Stream)
  ]
 where
  p :: Proxy a
  p = Proxy :: Proxy a
  f :: TypeScript a => Proxy a -> [TSDeclaration]
  f = getTypeScriptDeclarations
