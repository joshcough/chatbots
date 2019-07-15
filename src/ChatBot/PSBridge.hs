
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
    let p = (Proxy :: Proxy DbCommand)         in o p
  , let p = (Proxy :: Proxy DbQuote)           in o p
  ]
  where
  o p = order p (mkSumType p)
