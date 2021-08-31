{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent       (forkIO)
import           Control.Monad.Except     (runExceptT)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (runReaderT)
import           Data.Aeson               (FromJSON)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Foldable            (traverse_)
import           GHC.Generics             (Generic)
import           Network.Wai.Handler.Warp (run)
import qualified Network.Wreq.Session     as S
import           Servant                  (Application, Proxy (Proxy), Server,
                                           serve)
import           Servant.API              (Capture, JSON, NoContent (..), Post,
                                           QueryParam, ReqBody, (:<|>), (:>))
import           System.Exit              (ExitCode (ExitFailure), exitWith)

import           Config                   (Config (..), getConfig)
import           HandleTicket             (Commit (..), Error (..),
                                           TicketResult (..), handleTicket)

type API = ReqBody '[JSON] CommitInfo :> Post '[JSON] NoContent

data CommitInfo = CommitInfo
                 { ref     :: String
                 , commits :: [Commit]
                 }
                 deriving (Show, Generic)

instance FromJSON CommitInfo

commitAPI :: Proxy API
commitAPI = Proxy

app :: Config () -> Application
app = serve commitAPI . server

server :: Config () -> Server API
server config (CommitInfo branch commits) = do
  -- dropping the first 11 characters to get rid of "refs/heads/"
  let branchName = drop 11 branch
  session <- liftIO S.newAPISession

  liftIO $ traverse_ (\commitMessage -> forkIO $ do
      let config' = config { httpSession = session }
      result <- flip runReaderT config' . runExceptT $ handleTicket branchName commitMessage
      logTicketHandlingresult result
    ) commits

  return NoContent

main :: IO ()
main = do
  maybeConfig <- runExceptT getConfig
  case maybeConfig of
    Left err     -> do
      print err
      exitWith (ExitFailure 1)
    Right config -> do
      putStrLn "running on port 8080"
      run 8080 (app config)

logTicketHandlingresult :: Either Error TicketResult -> IO ()
logTicketHandlingresult (Left err) = print err
logTicketHandlingresult (Right (MovedTicket ticketId columnName)) = putStrLn $ "moved " <> ticketId <> " to " <> columnName
logTicketHandlingresult (Right (NoDesiredColumnFound branchName (Commit message))) = putStrLn $ "no desired state found, branch: '" <> branchName <> "', commit message: '" <> message <> "'"
