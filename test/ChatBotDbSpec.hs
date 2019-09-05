module ChatBotDbSpec (spec) where

import Protolude
import Test.Hspec

import ChatBot.Config (ChannelName(..))
import ChatBot.Models (Quote(..))
import ChatBot.Storage (QuotesDb(..), insertQuote)
import Helpers
import Types (runAppToIO)

art :: ChannelName
art = ChannelName "artoftroll"

daut :: ChannelName
daut = ChannelName "daut"

spec :: Spec
spec = withDB $
    describe "Quotes" $ do
        it "can insert quote" $ \(_, config) -> do
            qid <- runAppToIO config $
                insertQ art "i am so good at this game"
            qid `shouldBe` 0

        it "can insert many quotes in same channel" $ \(_, config) -> do
            (q1, q2) <- runAppToIO config $ do
                q1 <- insertQ art "look what i can do"
                q2 <- insertQ art "bam"
                pure (q1, q2)
            (q1, q2) `shouldBe` (0, 1)

        it "can insert many quotes in different channels" $ \(_, config) -> do
            (q1, q2, q3, q4) <- runAppToIO config $ do
                q1 <- insertQ art "look what i can do - art"
                q2 <- insertQ art "bam - art"
                q3 <- insertQ daut "look what i can do - daut"
                q4 <- insertQ daut "bam - daut"
                pure (q1, q2, q3, q4)
            (q1, q2, q3, q4) `shouldBe` (0, 1, 0, 1)

insertQ :: QuotesDb f => ChannelName -> Text -> f Int
insertQ c = fmap quoteQid . insertQuote c
