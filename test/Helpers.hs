module Helpers (withDB) where

import           Protolude

import           Config               (Config (..))
import           Test.Hspec
import           Util.DatabaseHelpers (getDatabase, truncateDb)

--
-- NOTE: if having trouble with db, do this: DBLOGGING=VERBOSE stack test
-- Also: for quieter tests: LOG_LEVEL=LevelError stack test
--

---
--- Setup and teardown helpers
---
withDB :: SpecWith (IO (), Config) -> Spec
withDB = beforeAll getDatabase . afterAll fst . after (truncateDb . snd)
