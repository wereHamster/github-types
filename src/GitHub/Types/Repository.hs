{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GitHub.Types.Repository where


import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Text

import GitHub.Types.Base



data CreateDeploymentStatusRequest = CreateDeploymentStatusRequest
    { cdsState       :: State
    , cdsTargetUrl   :: Maybe Text
    , cdsDescription :: Maybe Text
    } deriving (Eq, Show)


instance ToJSON CreateDeploymentStatusRequest where
    toJSON CreateDeploymentStatusRequest{..} = object
        [ "state"       .= cdsState
        , "target_url"  .= cdsTargetUrl
        , "description" .= cdsDescription
        ]


data Deployment = Deployment
    { deploymentId          :: Int
    , deploymentSha         :: Text
    , deploymentRef         :: Text
    , deploymentEnvironment :: Text
    , deploymentDescription :: Text
    , deploymentPayload     :: Value
    } deriving (Eq, Show)

instance FromJSON Deployment where
    parseJSON (Object x) = Deployment
        <$> x .: "id"
        <*> x .: "sha"
        <*> x .: "ref"
        <*> x .: "environment"
        <*> x .: "description"
        <*> x .: "payload"

    parseJSON _ = mzero
