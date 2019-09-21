module ChatBotMessageProcessorSpec (spec) where

import Protolude
import Test.Hspec

--import ChatBot.Config (ChannelName(..))
--import ChatBot.Models (Quote(..))
--import Helpers
--import Types (runAppToIO)
--import ChatBot.Storage (CommandsDb(..), QuotesDb(..), insertQuote)
--import Control.Monad.Trans.Writer (WriterT, tell)
--
--art :: ChannelName
--art = ChannelName "artoftroll"
--
--daut :: ChannelName
--daut = ChannelName "daut"
--
--
--type App = WriterT [RawIrcMsg] (AppTEnv' ChatBotError IO Config)
--
--instance QuotesDb App where
--    getStreams = lift getStreams
--    insertQuote c = lift . insertQuote c
--    getQuote c = lift . getQuote c
--    deleteQuote c = lift . deleteQuote c
--    getQuotes = lift . getQuotes
--
--instance CommandsDb App where
--    insertCommand c t = lift . insertCommand c t
--    getCommand c = lift . getCommand c
--    deleteCommand c = lift . deleteCommand c
--    getCommands = lift . getCommands
--
--instance Sender App where
--  send m = tell [m]
--

spec :: Spec
spec = return ()

--spec :: Spec
--spec = withDB $
--    describe "Quotes" $ do
--        it "can insert quote" $ \(_, config) -> do
--            qid <- runAppToIO config $
--                insertQ art "i am so good at this game"
--            qid `shouldBe` 1
--
--        it "can insert many quotes in same channel" $ \(_, config) -> do
--            (q1, q2) <- runAppToIO config $ do
--                q1 <- insertQ art "look what i can do"
--                q2 <- insertQ art "bam"
--                pure (q1, q2)
--            (q1, q2) `shouldBe` (1, 2)
--
--        it "can insert many quotes in different channels" $ \(_, config) -> do
--            (q1, q2, q3, q4) <- runAppToIO config $ do
--                q1 <- insertQ art "look what i can do - art"
--                q2 <- insertQ art "bam - art"
--                q3 <- insertQ daut "look what i can do - daut"
--                q4 <- insertQ daut "bam - daut"
--                pure (q1, q2, q3, q4)
--            (q1, q2, q3, q4) `shouldBe` (1, 2, 1, 2)
--
--insertQ :: QuotesDb f => ChannelName -> Text -> f Int
--insertQ c = fmap quoteQid . insertQuote c
