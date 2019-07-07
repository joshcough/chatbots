module ChatBotDbSpec (spec) where

import Protolude
import           Test.Hspec

import           ChatBot.Config (ChannelName(..))
import           ChatBot.DatabaseModels (DbQuote(..))
import           ChatBot.Storage (QuotesDb(..), insertQuote)
import           Database.Persist (Entity(..))
import           Helpers
import           Types (runAppToIO)

art :: ChannelName
art = ChannelName "artoftroll"

daut :: ChannelName
daut = ChannelName "daut"

spec :: Spec
spec = around setupTeardown $
    describe "Quotes" $ do
        it "can insert quote" $ \config -> do
            qid <- runAppToIO config $
                insertQ art "i am so good at this game"
            qid `shouldBe` 0

        it "can insert many quotes in same channel" $ \config -> do
            (q1, q2) <- runAppToIO config $ do
                q1 <- insertQ art "look what i can do"
                q2 <- insertQ art "bam"
                pure (q1, q2)
            (q1, q2) `shouldBe` (0, 1)

        it "can insert many quotes in different channels" $ \config -> do
            (q1, q2, q3, q4) <- runAppToIO config $ do
                q1 <- insertQ art "look what i can do - art"
                q2 <- insertQ art "bam - art"
                q3 <- insertQ daut "look what i can do - daut"
                q4 <- insertQ daut "bam - daut"
                pure (q1, q2, q3, q4)
            (q1, q2, q3, q4) `shouldBe` (0, 1, 0, 1)

insertQ :: QuotesDb f => ChannelName -> Text -> f Int
insertQ c = fmap (dbQuoteQid . entityVal) . insertQuote c
