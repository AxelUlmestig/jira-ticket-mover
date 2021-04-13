{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.UTF8 as BSU
import           Data.List            (find)
import           Network.Wreq
import           System.Environment   (lookupEnv)
import           System.Exit          (ExitCode (ExitFailure), exitWith)

data TransitionsResponse = TransitionsResponse {
  transitions :: [Transition]
} deriving (Show)

data Transition = Transition {
  id   :: String,
  name :: String
} deriving (Show)

instance FromJSON TransitionsResponse where
  parseJSON = withObject "transitionResponse" $ \o ->
    TransitionsResponse <$> o .: "transitions"

instance FromJSON Transition where
  parseJSON = withObject "transition" $ \o ->
    Transition <$> o .: "id" <*> o .: "name"

data JiraConfig = JiraConfig {
  requestOptions :: Network.Wreq.Options,
  jiraUrl        :: String
}

main :: IO ()
main = do
  config <- getJiraConfig
  let ticketId = "EXP-392"

  transitions <- getTransitions config ticketId
  -- transitionTo config transitions "Shippable" ticketId
  transitionTo config transitions "Done" ticketId

getEnvVariableOrExit :: String -> IO String
getEnvVariableOrExit varName = do
  maybeVar <- lookupEnv varName
  case maybeVar of
    Nothing -> do
      putStrLn (varName <> " env variable missing, exiting")
      exitWith (ExitFailure 1)
    (Just var) -> do
      return var

getJiraCredentialsOrExit :: IO (BSU.ByteString, BSU.ByteString)
getJiraCredentialsOrExit = do
  jiraUsername <- getEnvVariableOrExit "JIRA_USERNAME"
  jiraPassword <- getEnvVariableOrExit "JIRA_PASSWORD"
  return (BSU.fromString jiraUsername, BSU.fromString jiraPassword)

getJiraConfig :: IO JiraConfig
getJiraConfig = do
  jiraUrl <- getEnvVariableOrExit "JIRA_URL"
  (jiraUsername, jiraPassword) <- getJiraCredentialsOrExit
  let opts = set auth (Just (basicAuth jiraUsername jiraPassword)) defaults

  return JiraConfig {
      jiraUrl = jiraUrl,
      requestOptions = set (header "Content-Type") (["application/json"]) opts
    }

getTransitions :: JiraConfig -> String -> IO [Transition]
getTransitions config ticketId = do
  res <- fmap decode <$> getWith (requestOptions config) (jiraUrl config <> "/rest/api/2/issue/" <> ticketId <> "/transitions")
  case view (responseStatus . statusCode) res of
    status | status /= 200 -> do
      putStrLn ("failed to get transitions, status: " <> show status)
      exitWith (ExitFailure 1)
    status -> do
      -- TODO: handle this in a less hacky way
      let (Just (TransitionsResponse transitions)) = view responseBody res
      return transitions

transitionTo :: JiraConfig -> [Transition] -> String -> String -> IO ()
transitionTo config transitions state ticketId = do
  let maybeTransition = find ((==state) . name) transitions
  case maybeTransition of
    Nothing -> do
      print "desired transition not available"
      exitWith (ExitFailure 1)
    Just (Transition transitionId _) -> do
      let body = BSU.fromString $ "{\"transition\":{\"id\":\"" <> transitionId <> "\"}}"
      res <- postWith (requestOptions config) (jiraUrl config <> "/rest/api/2/issue/" <> ticketId <> "/transitions") body
      print res

