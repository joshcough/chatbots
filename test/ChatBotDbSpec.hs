module ChatBotDbSpec (spec) where

import Protolude
import Test.Hspec

import ChatBot.Config (ChannelName(..))
import ChatBot.Models (Question(..))
import ChatBot.Storage (QuestionsDb(..), insertQuestion)
import Helpers
import Types (runAppToIO)

art :: ChannelName
art = ChannelName "artoftroll"

daut :: ChannelName
daut = ChannelName "daut"

spec :: Spec
spec = withDB $ do
    describe "Questions" $ do
        it "can insert question" $ \(_, config) -> do
            qid <- runAppToIO config $
                insertQ art "i am so good at this game?"
            qid `shouldBe` 1

        it "can insert many questions in same channel" $ \(_, config) -> do
            (q1, q2) <- runAppToIO config $ do
                q1 <- insertQ art "look what i can do?"
                q2 <- insertQ art "bam?"
                pure (q1, q2)
            (q1, q2) `shouldBe` (1, 2)

        it "can insert many questions in different channels" $ \(_, config) -> do
            (q1, q2, q3, q4) <- runAppToIO config $ do
                q1 <- insertQ art "look what i can do - art?"
                q2 <- insertQ art "bam - art?"
                q3 <- insertQ daut "look what i can do - daut?"
                q4 <- insertQ daut "bam - daut?"
                pure (q1, q2, q3, q4)
            (q1, q2, q3, q4) `shouldBe` (1, 2, 1, 2)

    describe "Quotes" $ do
        it "can insert quote" $ \(_, config) -> do
            qid <- runAppToIO config $
                insertQ art "i am so good at this game"
            qid `shouldBe` 1

        it "can insert many quotes in same channel" $ \(_, config) -> do
            (q1, q2) <- runAppToIO config $ do
                q1 <- insertQ art "look what i can do"
                q2 <- insertQ art "bam"
                pure (q1, q2)
            (q1, q2) `shouldBe` (1, 2)

        it "can insert many quotes in different channels" $ \(_, config) -> do
            (q1, q2, q3, q4) <- runAppToIO config $ do
                q1 <- insertQ art "look what i can do - art"
                q2 <- insertQ art "bam - art"
                q3 <- insertQ daut "look what i can do - daut"
                q4 <- insertQ daut "bam - daut"
                pure (q1, q2, q3, q4)
            (q1, q2, q3, q4) `shouldBe` (1, 2, 1, 2)

insertQ :: QuestionsDb f => ChannelName -> Text -> f Int
insertQ c = fmap questionQid . insertQuestion c
