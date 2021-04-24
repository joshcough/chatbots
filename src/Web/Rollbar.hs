{-# LANGUAGE OverloadedStrings #-}

module Web.Rollbar
    ( -- * Reporting function
      rollbar
      -- * Types
    , module Web.Rollbar.Types
    ) where

import qualified Web.Rollbar.Internal as I
import Web.Rollbar.Types

import Control.Lens (view)
import Network.HTTP.Nano
    ( AsHttpError
    , HasHttpCfg
    , HttpMethod(POST)
    , addHeaders
    , buildReq
    , http'
    , mkJSONData
    )
import Protolude

-- | Post @Event@ to Rollbar error reporting service
rollbar ::
       ( MonadIO m
       , MonadError e m
       , MonadReader r m
       , AsHttpError e
       , HasHttpCfg r
       , HasRollbarCfg r
       , ToRollbarEvent evt
       )
    => evt -- ^ event to be posted
    -> m ()
rollbar evt = do
    isMuted <- view rollbarCfgMute
    unless isMuted $ do
        v <- I.encodeEvent $ toRollbarEvent evt
        http' . addHeaders [("Content-Type", "application/json")] =<<
            buildReq POST "https://api.rollbar.com/api/1/item/" (mkJSONData v)
