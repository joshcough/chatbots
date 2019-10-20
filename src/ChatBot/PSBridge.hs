
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
    go (Proxy :: Proxy ChannelName)
  , go (Proxy :: Proxy ChatUserName)
  , go (Proxy :: Proxy Command)
  , go (Proxy :: Proxy Question)
  , go (Proxy :: Proxy Quote)
  ]
  where
  go p = order p (mkSumType p)
