{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Rollbar.Types
    ( -- * Configuration
      RollbarCfg(..)
    , HasRollbarCfg(..)
      -- * Configuration types
    , AccessToken(AccessToken)
    , Environment(Environment)
    , CodeVersion(CodeVersion)
    , Host(Host)
      -- * Event
    , Event(..)
    , EventLevel(..)
    , ToRollbarEvent(..)
    , eventContext
    , eventData
    , eventLevel
    , eventMessage
    , eventTitle
    , eventUUID
    ) where

import Control.Lens.TH (makeClassy, makeLenses)
import Data.Aeson (ToJSON(..), Value(String))
import Protolude

-- | Grants access to the Rollbar API. Can be retrieved from
-- @https://rollbar.com/<org>/<project>/settings/access_tokens/@
newtype AccessToken = AccessToken
    { unAccessToken :: Text
    }

instance ToJSON AccessToken where
    toJSON = toJSON . unAccessToken

-- | Environment in which the code runs, e.g. @production@,
-- @staging@, @development@, etc.
newtype Environment = Environment
    { unEnvironment :: Text
    }

instance ToJSON Environment where
    toJSON = toJSON . unEnvironment

-- | Version of the code that is running.
--
-- Max length: 40 characters
--
-- Rollbar understands these formats:
--
--      * semantic version, e.g. @"2.1.12"@
--      * integer, e.g. @"42"@
--      * Git SHA, e.g. @"3da541559918a808c2402bba5012f6c60b27661c"@
newtype CodeVersion = CodeVersion
    { unCodeVersion :: Text
    }

instance ToJSON CodeVersion where
    toJSON = toJSON . unCodeVersion

-- | Server hostname, indexed by Rollbar. Can be used to describe the service
-- name, e.g. @apiary@, @gourd@, @trellis@, etc.
--
-- Rollbar allows search by prefix, e.g. searching @apiary@ will find both
-- @apiary-us-east-1@ and @apiary-us-west-2@.
newtype Host = Host
    { unHost :: Text
    }

instance ToJSON Host where
    toJSON = toJSON . unHost

-- | Configuration used to post events to the Rollbar API.
data RollbarCfg = RollbarCfg
    { _rollbarCfgToken :: !AccessToken
    , _rollbarCfgEnvironment :: !Environment
    , _rollbarCfgHost :: !(Maybe Host)
    , _rollbarCfgCodeVersion :: !(Maybe CodeVersion)
    , _rollbarCfgMute :: !Bool
    }

-- | Event that is posted to Rollbar.
-- See: https://docs.rollbar.com/reference#section-data-format
data Event = Event
    { -- | Severity level
      _eventLevel :: !EventLevel
      -- | Uniquely identifies this occurrence.
      -- We recommend using a UUID4 (16 random bytes).
      --
      -- Max length: 36 characters
      --
      -- The UUID space is unique to each project and can be used to look up an
      -- occurrence later.
      -- The UUID is also used to detect duplicate requests. If you send the
      -- same UUID in two payloads, the second one will be discarded.
      -- While optional, it is recommended that all clients generate and provide
      -- this field.
      -- /TODO: Pick better type and generate this internally./
    , _eventUUID :: !(Maybe Text)
    -- | Title of the event by which occurrences will be grouped by.
    --
    -- Max length: 255 characters
    , _eventTitle :: !Text
    -- | Primary message text
    , _eventMessage :: !Text
    -- | Custom JSON metadata to help you debug this event.
    , _eventData :: !(Maybe Value)
    -- | Identifier for which part of your application this event came from.
    -- Items can be searched by context (prefix search).
    -- For example, the current API or route, e.g. @api\/user\/v2\/search@.
    , _eventContext :: !(Maybe Text)
    }

-- | Typeclass for converting a type to a Rollbar 'Event'.
class ToRollbarEvent e where
    toRollbarEvent :: e -> Event

instance ToRollbarEvent Event where
    toRollbarEvent = identity

-- | Severity level of an event.
data EventLevel
    = Debug
    | Info
    | Warning
    | Error
    | Critical
    deriving (Eq, Ord, Bounded, Show)

instance ToJSON EventLevel where
    toJSON Debug = String "debug"
    toJSON Info = String "info"
    toJSON Warning = String "warning"
    toJSON Error = String "error"
    toJSON Critical = String "critical"

makeClassy ''RollbarCfg

makeLenses ''Event
