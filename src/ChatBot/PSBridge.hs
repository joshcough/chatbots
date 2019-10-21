
module ChatBot.PSBridge where

import Protolude
import Language.PureScript.Bridge
import Language.PureScript.Bridge.SumType (order)
import ChatBot.Models

main :: IO ()
main = writePSTypes "frontend/src" (buildBridge defaultBridge) myTypes

-- TODO: there are more types in Models that maybe should be added here
myTypes :: [SumType 'Haskell]
myTypes = [
    o (Proxy :: Proxy ChannelName)
  , e (Proxy :: Proxy ChatUser)
  , o (Proxy :: Proxy ChatUserName)
  , o (Proxy :: Proxy Command)
  , o (Proxy :: Proxy Question)
  , o (Proxy :: Proxy Quote)
  ]
  where
  o p = order p (mkSumType p)
  e p = equal p (mkSumType p)
