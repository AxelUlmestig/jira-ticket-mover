{-# LANGUAGE OverloadedStrings #-}

module Config (
  ConfigError(..),
  Config(..),
  getConfig
) where


import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as BSC8
import qualified Data.ByteString.UTF8       as BSU
import           Data.Char                  (toLower)
import qualified Data.Map.Strict            as M
import           Network.Wreq
import           System.Environment         (lookupEnv)

data Config = Config {
  requestOptions :: Network.Wreq.Options,
  jiraUrl        :: String,
  secret         :: BSU.ByteString,
  desiredColumn  :: String -> String -> Maybe String
}

data ConfigError = MissingEnvVar String
                 | BranchColumnMappingParseError String
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

getBranchColumnMapping :: ExceptT ConfigError IO (String -> String -> Maybe String)
getBranchColumnMapping = do
  -- TODO: catch errors from `LBS.readFile`. `catchE` from Control.Monad.Trans.Except ?
  fileContents <- lift $ LBS.readFile "./branch-column-mapping.json"
  case decode fileContents of
    Nothing   -> except $ Left $ BranchColumnMappingParseError (BSC8.unpack fileContents)
    Just dict -> return $ \branch proj -> M.lookup (fmap toLower branch) dict >>= M.lookup (fmap toLower proj)

getConfig :: ExceptT ConfigError IO Config
getConfig = do
  jiraUrl <- getEnvVariable "JIRA_URL"
  (jiraUsername, jiraPassword) <- getJiraCredentials
  secret <- getEnvVariable "SECRET"
  let opts = set auth (Just (basicAuth jiraUsername jiraPassword)) defaults
  branchColumnMapping <- getBranchColumnMapping

  return Config {
      jiraUrl         = jiraUrl,
      secret          = BSU.fromString secret,
      requestOptions  = set (header "Content-Type") (["application/json"]) opts,
      desiredColumn   = branchColumnMapping
    }
