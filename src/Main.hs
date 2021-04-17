{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Except
import           Control.Monad.Reader
import           System.Exit          (ExitCode (ExitFailure), exitWith)

import           Config               (Config (..), getConfig)
import           HandleTicket         (Error (..), handleTicket)

main :: IO ()
main = do
  maybeConfig <- runExceptT getConfig
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
