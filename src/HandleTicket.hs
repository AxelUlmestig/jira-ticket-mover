{-# LANGUAGE OverloadedStrings #-}

module HandleTicket (
  handleTicket,
  Error(..),
  TicketResult(..)
) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.ByteString.UTF8       as BSU
import           Data.List                  (find)
import           Network.Wreq
import qualified Network.Wreq.Session       as S
import           Text.Regex.PCRE

import           Config                     (Config (..))

type TicketId = String
type ColumnName = String
type CommitMessage = String
type BranchName = String
type ProjectPrefix = String

data TicketResult = MovedTicket TicketId ColumnName
                  | NoDesiredColumnFound BranchName CommitMessage
                  deriving (Eq, Show)

data Error = GetTransitionsFailure Int
           | TransitionUnavailable ColumnName [Transition]
           | TransitionFailure Int
           deriving (Eq, Show)

type MonadStack a = ExceptT Error (ReaderT (Config S.Session) IO) a

newtype TransitionsResponse = TransitionsResponse {
  transitions :: [Transition]
} deriving (Eq, Show)

data Transition = Transition {
  id   :: String,
  name :: ColumnName
} deriving (Eq, Show)

instance FromJSON TransitionsResponse where
  parseJSON = withObject "transitionResponse" $ \o ->
    TransitionsResponse <$> o .: "transitions"

instance FromJSON Transition where
  parseJSON = withObject "transition" $ \o ->
    Transition <$> o .: "id" <*> o .: "name"

handleTicket :: ColumnName -> CommitMessage -> MonadStack TicketResult
handleTicket branch commitMessage = do
  getDesiredColumn <- asks desiredColumn
  case getTicketIdAndJiraColumn (getDesiredColumn branch) commitMessage of
    Nothing -> return $ NoDesiredColumnFound branch commitMessage
    Just (ticketId, jiraColumn) -> do
      transitions <- getTransitions ticketId
      transition  <- except (findTransition jiraColumn transitions)
      transitionTo transition ticketId

getTicketIdAndJiraColumn :: (ProjectPrefix -> Maybe ColumnName) -> CommitMessage -> Maybe (TicketId, ColumnName)
getTicketIdAndJiraColumn getDesiredColumn commitMessage = do
  (ticketId, projectPrefix) <- parseTicketInfo commitMessage
  jiraColumn <- getDesiredColumn projectPrefix
  return (ticketId, jiraColumn)

parseTicketInfo :: CommitMessage -> Maybe (TicketId, ProjectPrefix)
parseTicketInfo commitMessage =
  case commitMessage =~ regex of
    ((_:ticketId:projectPrefix:_):_) -> Just (ticketId, projectPrefix)
    _                                -> Nothing
  where
    regex = "^ *\\[(([a-zA-Z]+)-\\d+)\\]"::String

getTransitions :: TicketId -> MonadStack [Transition]
getTransitions ticketId = do
  baseUrl <- asks jiraUrl
  options <- asks requestOptions
  session <- asks httpSession
  res <- lift . lift $ fmap decode <$> S.getWith options session (baseUrl <> "/rest/api/2/issue/" <> ticketId <> "/transitions")
  case view (responseStatus . statusCode) res of
    status | status /= 200 -> except $ Left $ GetTransitionsFailure status
    _ -> do
      -- TODO: handle this in a less hacky way
      let (Just (TransitionsResponse transitions)) = view responseBody res
      return transitions

findTransition :: ColumnName -> [Transition] -> Either Error Transition
findTransition state transitions = do
  let maybeTransition = find ((==state) . name) transitions
  case maybeTransition of
    Nothing           -> Left $ TransitionUnavailable state transitions
    (Just transition) -> Right transition

transitionTo :: Transition -> TicketId -> MonadStack TicketResult
transitionTo (Transition transitionId state) ticketId = do
  let body = BSU.fromString $ "{\"transition\":{\"id\":\"" <> transitionId <> "\"}}"
  baseUrl <- asks jiraUrl
  options <- asks requestOptions
  session <- asks httpSession
  res <- lift . lift $ S.postWith options session (baseUrl <> "/rest/api/2/issue/" <> ticketId <> "/transitions") body
  case view (responseStatus . statusCode) res of
    status | status `div` 100 /= 2 -> except $ Left $ TransitionFailure status
    _                              -> return $ MovedTicket ticketId state

