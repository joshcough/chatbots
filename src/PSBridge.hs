
module PSBridge where

import qualified Auth.PSBridge as Auth
import qualified ChatBot.PSBridge as ChatBot
import Protolude

main :: IO ()
main = do
    Auth.main
    ChatBot.main
