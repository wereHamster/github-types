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
    , deploymentTask        :: Text
    , deploymentName        :: Text
    , deploymentEnvironment :: Text
    , deploymentPayload     :: Value
    , deploymentDescription :: Text
    } deriving (Eq, Show)

instance FromJSON Deployment where
    parseJSON (Object x) = Deployment
        <$> x .: "id"
        <*> x .: "sha"
        <*> x .: "ref"
        <*> x .: "task"
        <*> x .: "name"
        <*> x .: "environment"
        <*> x .: "payload"
        <*> x .: "description"

    parseJSON _ = mzero


data DeploymentStatus = DeploymentStatus
    { deploymentStatusId             :: Int
    , deploymentStatusState          :: Text
    , deploymentStatusTargetUrl      :: Maybe Text
    , deploymentStatusDescription    :: Maybe Text
    , deploymentStatusDeploymentUrl  :: Maybe Text
    } deriving (Eq, Show)

instance FromJSON DeploymentStatus where
    parseJSON (Object x) = DeploymentStatus
        <$> x .: "id"
        <*> x .: "state"
        <*> x .: "target_url"
        <*> x .: "description"
        <*> x .: "deployment_url"

    parseJSON _ = mzero
