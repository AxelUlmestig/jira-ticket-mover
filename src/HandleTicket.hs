{-# LANGUAGE OverloadedStrings #-}

module HandleTicket (
  handleTicket,
  Error(..)
) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.ByteString.UTF8       as BSU
import           Data.List                  (find)
import           Network.Wreq

import           Config                     (Config (..))

data Error = GetTransitionsFailure Int
           | TransitionUnavailable String [Transition]
           | TransitionFailure Int
           deriving (Eq, Show)

type MonadStack a = ExceptT Error (ReaderT Config IO) a

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

