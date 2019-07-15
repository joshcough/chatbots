
module PSBridge where

import Protolude
import qualified Auth.PSBridge     as Auth
import qualified ChatBot.PSBridge as ChatBot

main :: IO ()
main = do
    Auth.main
    ChatBot.main
