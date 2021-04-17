{-# LANGUAGE OverloadedStrings #-}

module Config (
  ConfigError(..),
  Config(..),
  getConfig
) where


import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import qualified Data.ByteString.UTF8       as BSU
import           Network.Wreq
import           System.Environment         (lookupEnv)

data Config = Config {
  requestOptions :: Network.Wreq.Options,
  jiraUrl        :: String
}

data ConfigError = MissingEnvVar String
                 deriving (Show)

getEnvVariable :: String -> ExceptT ConfigError IO String
getEnvVariable varName = do
  maybeVar <- lift $ lookupEnv varName
  case maybeVar of
    Nothing    -> except $ Left $ MissingEnvVar varName
    (Just var) -> return var

getJiraCredentials :: ExceptT ConfigError IO (BSU.ByteString, BSU.ByteString)
getJiraCredentials = do
  jiraUsername <- getEnvVariable "JIRA_USERNAME"
  jiraPassword <- getEnvVariable "JIRA_PASSWORD"
  return (BSU.fromString jiraUsername, BSU.fromString jiraPassword)

getConfig :: ExceptT ConfigError IO Config
getConfig = do
  jiraUrl <- getEnvVariable "JIRA_URL"
  (jiraUsername, jiraPassword) <- getJiraCredentials
  let opts = set auth (Just (basicAuth jiraUsername jiraPassword)) defaults

  return Config {
      jiraUrl = jiraUrl,
      requestOptions = set (header "Content-Type") (["application/json"]) opts
    }
