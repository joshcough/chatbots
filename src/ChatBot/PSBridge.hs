
module ChatBot.PSBridge where

import Protolude
import Language.PureScript.Bridge
import Language.PureScript.Bridge.SumType (order)
import ChatBot.DatabaseModels

main :: IO ()
main = writePSTypes "frontend/src" (buildBridge defaultBridge) myTypes

-- TODO: there are more types in Models that maybe should be added here
myTypes :: [SumType 'Haskell]
myTypes = [
    go (Proxy :: Proxy DbCommand)
  , go (Proxy :: Proxy DbQuote)
  ]
  where
  go p = order p (mkSumType p)
