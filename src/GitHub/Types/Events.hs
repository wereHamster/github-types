{-# LANGUAGE OverloadedStrings #-}

module GitHub.Types.Events where


import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import Data.Text
import Data.Time

import GitHub.Types.Base
import GitHub.Types.Repository



-- | All events which can be produced by GitHub.
--
-- See https://developer.github.com/v3/activity/events/
data Event = Event
    { eventId :: !Text
    , eventActor :: !EventActor
    , eventRepo :: !EventRepo
    , eventCreatedAt :: !UTCTime
    , eventIsPublic :: !Bool
    , eventPayload :: !EventPayload
    } deriving (Eq, Show)

instance FromJSON Event where
    parseJSON (Object o) = do
        eventType <- o .: "type"

        Event
          <$> o .: "id"
          <*> o .: "actor"
          <*> o .: "repo"
          <*> o .: "created_at"
          <*> o .: "public"
          <*> (eventPayloadParser eventType =<< o .: "payload")

    parseJSON _ = fail "Event"


data EventActor = EventActor
    { eventActorId :: !Text
    , eventActorLogin :: !Text
    } deriving (Eq, Show)

instance FromJSON EventActor where
    parseJSON (Object o) = EventActor
        <$> o .: "id"
        <*> o .: "login"

    parseJSON _ = fail "EventActor"


data EventRepo = EventRepo
    { eventRepoId :: !Text
    , eventRepoName :: !Text
    } deriving (Eq, Show)

instance FromJSON EventRepo where
    parseJSON (Object o) = EventRepo
        <$> o .: "id"
        <*> o .: "name"

    parseJSON _ = fail "EventRepo"


data EventPayload
    = CommitCommentEventType    CommitCommentEvent
    | DeploymentEventType       DeploymentEvent
    | DeploymentStatusEventType DeploymentStatusEvent
    deriving (Eq, Show)


eventPayloadParser :: Text -> Value -> Parser EventPayload
eventPayloadParser "CommitCommentEvent"    x = CommitCommentEventType    <$> parseJSON x
eventPayloadParser "DeploymentEvent"       x = DeploymentEventType       <$> parseJSON x
eventPayloadParser "DeploymentStatusEvent" x = DeploymentStatusEventType <$> parseJSON x
eventPayloadParser eventType           _ = fail $ "eventPayloadParser: Unknown event type: " <> unpack eventType

-- | Since the event type is included through different means (X-GitHub-Event
-- header, or inline in the JSON object), it's not possible to make 'Event'
-- an instance of 'FromJSON'. But if you know the type, you can use this
-- parser.
eventParser :: Text -> Value -> Parser EventPayload
eventParser "commit_comment"    x = CommitCommentEventType    <$> parseJSON x
eventParser "deployment"        x = DeploymentEventType       <$> parseJSON x
eventParser "deployment_status" x = DeploymentStatusEventType <$> parseJSON x
eventParser eventType           _ = fail $ "Unknown event type: " <> unpack eventType



------------------------------------------------------------------------------
-- CommitCommentEvent

data CommitCommentEvent = CommitCommentEvent
    { commitCommentEventRepository :: Repository
    } deriving (Eq, Show)

instance FromJSON CommitCommentEvent where
    parseJSON (Object x) = CommitCommentEvent
        <$> x .: "repository"

    parseJSON _ = fail "CommitCommentEvent"


------------------------------------------------------------------------------
-- DeploymentEvent

data DeploymentEvent = DeploymentEvent
    { deploymentEventDeployment  :: Deployment
      -- ^ The deployment.
    , deploymentEventRepository  :: Repository
      -- ^ The repository for which the deployment was created (UNDOCUMENTED).
    } deriving (Eq, Show)

instance FromJSON DeploymentEvent where
    parseJSON (Object x) = DeploymentEvent
        <$> x .: "deployment"
        <*> x .: "repository"

    parseJSON _ = fail "DeploymentEvent"


------------------------------------------------------------------------------
-- DeploymentStatusEvent

data DeploymentStatusEvent = DeploymentStatusEvent
    { deploymentStatusEventDeploymentStatus :: DeploymentStatus
      -- ^ The deployment status.
    , deploymentStatusEventDeployment       :: Deployment
      -- ^ The deployment which the status affects.
    , deploymentStatusEventRepository       :: Repository
      -- ^ The repository for which the deployment was created (UNDOCUMENTED).
    } deriving (Eq, Show)

instance FromJSON DeploymentStatusEvent where
    parseJSON (Object x) = DeploymentStatusEvent
        <$> x .: "deployment_status"
        <*> x .: "deployment"
        <*> x .: "repository"

    parseJSON _ = fail "DeploymentStatusEvent"
