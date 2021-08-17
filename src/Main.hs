{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent       (forkIO)
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy     as LBS
import           Data.Foldable            (traverse_)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Internal
import qualified Network.Wreq.Session     as S
import           System.Exit              (ExitCode (ExitFailure), exitWith)

import           Config                   (Config (..), getConfig)
import           HandleTicket             (Error (..), TicketResult (..),
                                           handleTicket)
import           ParseCommits             (CommitInfo (..), parseCommits)

main :: IO ()
main = do
  maybeConfig <- runExceptT getConfig
  case maybeConfig of
    Left err         -> do
      print err
      exitWith (ExitFailure 1)
    Right config -> do
      putStrLn "running on port 8080"
      run 8080 (app config)

app :: Config () -> Application
app config req@Request{requestMethod="POST", rawPathInfo="/"} respond = do
  body <- consumeRequestBodyLazy req
  case parseCommits body of
    Nothing                           -> respond $ responseLBS status400 [] ""
    Just (CommitInfo branch messages) -> do
      session <- S.newAPISession
      traverse_ (\commitMessage -> forkIO $ do
          let config' = config { httpSession = session }
          result <- flip runReaderT config' . runExceptT $ handleTicket branch commitMessage
          logTicketHandlingresult result
        ) messages
      respond $ responseLBS status200 [] ""
app _ _ respond = respond $ responseLBS status404 [] ""

logTicketHandlingresult :: Either Error TicketResult -> IO ()
logTicketHandlingresult (Left err) = print err
logTicketHandlingresult (Right (MovedTicket ticketId columnName)) = putStrLn $ "moved " <> ticketId <> " to " <> columnName
logTicketHandlingresult (Right (NoDesiredColumnFound branchName commitMessage)) = putStrLn $ "no desired state found, branch: '" <> branchName <> "', commit message: '" <> commitMessage <> "'"
