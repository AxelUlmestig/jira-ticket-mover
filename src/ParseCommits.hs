{-# LANGUAGE OverloadedStrings #-}

module ParseCommits (
  CommitInfo(..),
  parseCommits
) where

import           Control.Monad        ((>=>))
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T
import           Data.Vector          (toList)

data CommitInfo = CommitInfo {
                    branchName     :: String,
                    commitMessages :: [String]
                  }
                  deriving (Eq, Show)
{-
  expecting data to look like this:

  {
    "ref": "refs/heads/master",
    "commits": [
      "message": "foo bar"
    ]
  }
-}
parseCommits :: LBS.ByteString -> Maybe CommitInfo
parseCommits requestBody = do
  body    <- (decode requestBody) >>= obj
  branch  <- M.lookup "ref" body >>= str
  commits <- M.lookup "commits" body >>= arr
  commitMessages <- traverse (obj >=> M.lookup "message" >=> str) commits
  return (CommitInfo branch commitMessages)

obj :: Value -> Maybe (M.HashMap T.Text Value)
obj (Object o) = Just o
obj _          = Nothing

arr :: Value -> Maybe [Value]
arr (Array a) = Just (toList a)
arr _         = Nothing

str :: Value -> Maybe String
str (String s) = Just (T.unpack s)
str _          = Nothing
