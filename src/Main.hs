{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.ByteString.UTF8       as BSU
import           Data.List                  (find)
import           Network.Wreq
import           System.Environment         (lookupEnv)
import           System.Exit                (ExitCode (ExitFailure), exitWith)

data ConfigError = MissingEnvVar String
                 deriving (Show)

data Error = GetTransitionsFailure Int
           | TransitionUnavailable String [Transition]
           | TransitionFailure Int
           deriving (Eq, Show)

type MonadStack a = ExceptT Error (ReaderT JiraConfig IO) a

---

data TransitionsResponse = TransitionsResponse {
  transitions :: [Transition]
} deriving (Eq, Show)

data Transition = Transition {
  id   :: String,
  name :: String
} deriving (Eq, Show)

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

handleTicket :: String -> MonadStack ()
handleTicket ticketId = do
  transitions <- getTransitions ticketId
  transition  <- except (findTransition "Shippable" transitions)
  transitionTo transition ticketId

getTransitions :: String -> MonadStack [Transition]
getTransitions ticketId = do
  baseUrl <- asks jiraUrl
  options <- asks requestOptions
  res <- lift . lift $ fmap decode <$> getWith options (baseUrl <> "/rest/api/2/issue/" <> ticketId <> "/transitions")
  case view (responseStatus . statusCode) res of
    status | status /= 200 -> except $ Left $ GetTransitionsFailure status
    _ -> do
      -- TODO: handle this in a less hacky way
      let (Just (TransitionsResponse transitions)) = view responseBody res
      return transitions

findTransition :: String -> [Transition] -> Either Error Transition
findTransition state transitions = do
  let maybeTransition = find ((==state) . name) transitions
  case maybeTransition of
    Nothing           -> Left $ TransitionUnavailable state transitions
    (Just transition) -> Right $ transition

transitionTo :: Transition -> String -> MonadStack ()
transitionTo (Transition transitionId _) ticketId = do
  let body = BSU.fromString $ "{\"transition\":{\"id\":\"" <> transitionId <> "\"}}"
  baseUrl <- asks jiraUrl
  options <- asks requestOptions
  res <- lift . lift $ postWith options (baseUrl <> "/rest/api/2/issue/" <> ticketId <> "/transitions") body
  case view (responseStatus . statusCode) res of
    status | status `div` 100 /= 2 -> except $ Left $ TransitionFailure status
    _                              -> return ()

main :: IO ()
main = do
  maybeConfig <- runExceptT getJiraConfig
  case maybeConfig of
    Left err         -> do
      print err
      exitWith (ExitFailure 1)
    Right jiraConfig -> do
      let ticketId = "EXP-392"
      result <- flip runReaderT jiraConfig . runExceptT $ handleTicket ticketId
      case result of
        Left err ->  do
          print err
          exitWith (ExitFailure 1)
        Right _  -> putStrLn "succes \\o/"

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

getJiraConfig :: ExceptT ConfigError IO JiraConfig
getJiraConfig = do
  jiraUrl <- getEnvVariable "JIRA_URL"
  (jiraUsername, jiraPassword) <- getJiraCredentials
  let opts = set auth (Just (basicAuth jiraUsername jiraPassword)) defaults

  return JiraConfig {
      jiraUrl = jiraUrl,
      requestOptions = set (header "Content-Type") (["application/json"]) opts
    }
