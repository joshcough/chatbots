{-# LANGUAGE OverloadedStrings #-}

module Web.Rollbar.Internal where

import Control.Lens ((^.), view)
import Data.Aeson (Value, (.=), object)
import Protolude
import Web.Rollbar.Types
    ( Event
    , HasRollbarCfg
    , eventContext
    , eventData
    , eventLevel
    , eventMessage
    , eventTitle
    , eventUUID
    , rollbarCfgCodeVersion
    , rollbarCfgEnvironment
    , rollbarCfgHost
    , rollbarCfgToken
    )

-- | Encode an @Event@ Documentation: https://docs.rollbar.com/reference
encodeEvent :: (MonadReader r m, HasRollbarCfg r) => Event -> m Value
encodeEvent evt = do
    token <- view rollbarCfgToken
    env <- view rollbarCfgEnvironment
    host <- view rollbarCfgHost
    codeVersion <- view rollbarCfgCodeVersion
    return . object $
        [ "access_token" .= token
        , "data" .=
          object
              ([ "environment" .= env
               , "level" .= (evt ^. eventLevel)
               , "title" .= (evt ^. eventTitle)
               , "body" .=
                 object
                     [ "message" .=
                       object ["body" .= (evt ^. eventMessage), "data" .= (evt ^. eventData)]
                     ]
               , "server" .= object (catMaybes [("host" .=) <$> host])
               , "notifier" .=
                 object ["name" .= ("cv-rollbar-haskell" :: Text), "version" .= ("0.2.0" :: Text)]
               ] <>
               catMaybes
                   [ ("uuid" .=) <$> evt ^. eventUUID
                   , ("context" .=) <$> evt ^. eventContext
                   , ("code_version" .=) <$> codeVersion
                   ])
        ]
