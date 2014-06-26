{-# LANGUAGE OverloadedStrings #-}

module GitHub.Types.Events where


import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Text

import GitHub.Types.Base
import GitHub.Types.Repository



data Event
    = CommitCommentEventType    CommitCommentEvent
    | DeploymentEventType       DeploymentEvent
    | DeploymentStatusEventType DeploymentStatusEvent
    deriving (Eq, Show)



------------------------------------------------------------------------------
-- CommitCommentEvent

data CommitCommentEvent = CommitCommentEvent
    { commitCommentEventRepository :: Repository
    } deriving (Eq, Show)


------------------------------------------------------------------------------
-- DeploymentEvent

data DeploymentEvent = DeploymentEvent
    { deploymentEventId          :: Int
      -- ^ The deployment Id (UNDOCUMENTED).
    , deploymentEventSha         :: Text
      -- The commit SHA for which this deployment was created.
    , deploymentEventName        :: Text
      -- ^ Name of repository for this deployment, formatted as :owner/:repo.
    , deploymentEventPayload     :: Value
      -- ^ The optional extra information for this deployment.
    , deploymentEventEnvironment :: Text
      -- ^ The optional environment to deploy to. Default: "production"
    , deploymentEventDescription :: Maybe Text
      -- ^ The optional human-readable description added to the deployment.
    , deploymentEventRepository  :: Repository
      -- ^ The repository for which the deployment was created (UNDOCUMENTED).
    } deriving (Eq, Show)

instance FromJSON DeploymentEvent where
    parseJSON (Object x) = DeploymentEvent
        <$> x .: "id"
        <*> x .: "sha"
        <*> x .: "name"
        <*> x .: "payload"
        <*> x .: "environment"
        <*> x .: "description"
        <*> x .: "repository"

    parseJSON _ = mzero


------------------------------------------------------------------------------
-- DeploymentStatusEvent

data DeploymentStatusEvent = DeploymentStatusEvent
    { deploymentStatusEventState       :: State
      -- ^ The new state.
    , deploymentStatusEventTargetUrl   :: Maybe Text
      -- ^ The optional link added to the status.
    , deploymentStatusEventDeployment  :: Deployment
      -- ^ The deployment that this status is associated with.
    , deploymentStatusEventDescription :: Maybe Text
      -- ^ The optional human-readable description added to the status.
    } deriving (Eq, Show)

instance FromJSON DeploymentStatusEvent where
    parseJSON (Object x) = DeploymentStatusEvent
        <$> x .: "state"
        <*> x .: "target_url"
        <*> x .: "deployment"
        <*> x .: "description"

    parseJSON _ = mzero
