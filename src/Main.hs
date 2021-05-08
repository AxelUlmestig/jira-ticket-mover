{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent        (forkIO)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Crypto.Hash.SHA256        (hmac)
import qualified Data.ByteString           as BS
import           Data.ByteString.Builder   (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy      as LBS
import           Data.ByteString.Lazy.UTF8 (fromString)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp  (run)
import           Network.Wai.Internal
import           System.Exit               (ExitCode (ExitFailure), exitWith)

import           Config                    (Config (..), getConfig)
import           HandleTicket              (Error (..), handleTicket)
import           ParseCommits              (CommitInfo (..), parseCommits)

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
app config req@Request{requestMethod="POST", rawPathInfo="/"} respond = do
  body <- consumeRequestBodyLazy req
  if getGitHubSignature req /= Just (hmacBody (secret config) (LBS.toStrict body))
    -- then respond $ responseLBS status401 [] ""
    then respond $ responseLBS status401 [] (fromString (show ((getGitHubSignature req, hmacBody (secret config) (LBS.toStrict body)))))
  else
    case parseCommits body of
      Nothing                           -> respond $ responseLBS status400 [] ""
      Just (CommitInfo branch messages) -> do
        flip traverse messages (\commitMessage -> forkIO $ do
            result <- flip runReaderT config . runExceptT $ handleTicket branch commitMessage
            logTicketHandlingresult result
          )
        respond $ responseLBS status200 [] ""
app _ _ respond = respond $ responseLBS status404 [] ""

logTicketHandlingresult :: Either Error () -> IO ()
logTicketHandlingresult (Left err) = print err
logTicketHandlingresult (Right ()) = return ()

getGitHubSignature :: Request -> Maybe BS.ByteString
getGitHubSignature = lookup "X-Hub-Signature-256" . requestHeaders

hmacBody :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmacBody sec body =
  let
    hex = LBS.toStrict $ toLazyByteString $ byteStringHex $ hmac sec body
  in
    "sha256=" <> hex
