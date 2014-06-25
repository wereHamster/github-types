{-# LANGUAGE OverloadedStrings #-}

module GitHub.Types.Base where


import Control.Applicative
import Control.Monad

import Data.Aeson hiding (Error, Success)
import Data.Text



------------------------------------------------------------------------------
-- Repository

data Repository = Repository
    { repositoryOwnerName :: Text
    , repositoryName      :: Text
    } deriving (Eq, Show)


instance FromJSON Repository where
    parseJSON (Object x) = Repository
        <$> (x .: "owner" >>= (.: "login"))
        <*> x .: "name"

    parseJSON _ = mzero



------------------------------------------------------------------------------
-- State

data State = Pending | Success | Failure | Error
    deriving (Eq, Show)


instance FromJSON State where
    parseJSON (String "pending") = return Pending
    parseJSON (String "success") = return Success
    parseJSON (String "failure") = return Failure
    parseJSON (String "error")   = return Error
    parseJSON _                  = mzero

instance ToJSON State where
    toJSON Pending = String "pending"
    toJSON Success = String "success"
    toJSON Failure = String "failure"
    toJSON Error   = String "error"
