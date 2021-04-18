{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy     as LBS
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Internal
import           System.Exit              (ExitCode (ExitFailure), exitWith)

import           Config                   (Config (..), getConfig)
import           HandleTicket             (Error (..), handleTicket)
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

app :: Config -> Application
app config Request{requestMethod="POST", rawPathInfo="/", requestBody=ioBody} respond = do
  -- TODO: body is just a chunk of the total body. Figure out how to get it all
  body <- ioBody
  case parseCommits (LBS.fromStrict body) of
    Nothing                           -> respond $ responseLBS status400 [] ""
    Just (CommitInfo branch messages) -> do
      flip traverse messages (\commitMessage -> do
          result <- flip runReaderT config . runExceptT $ handleTicket branch commitMessage
          logTicketHandlingresult result
        )
      respond $ responseLBS status200 [] ""
app _ _ respond = respond $ responseLBS status404 [] ""

logTicketHandlingresult :: Either Error () -> IO ()
logTicketHandlingresult (Left err) = print err
logTicketHandlingresult (Right ()) = return ()
