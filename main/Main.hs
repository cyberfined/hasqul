{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main (main) where

import Control.Exception         (bracket)
import Control.Monad             (when)
import Data.Int                  (Int32)
import Data.Text                 (Text)
import Database.Hasqul
import Hasql.Connection
import Hasql.Session      hiding (sql)
import Hasql.Statement
import System.Environment        (getProgName, getArgs)
import System.Exit               (exitFailure)

import qualified Data.ByteString as BS

data Test = Test
    { testKey         :: !(Key Test)
    , testAge         :: !Int32
    , testDescription :: !Text
    , testAuthor      :: !(Maybe Text)
    , testUnused      :: ![Int]
    } deriving stock (Generic, Show)
      deriving Codec via (Table '[IgnoreField "testUnused"] Test)

dbSettings :: Settings
dbSettings = settings "127.0.0.1" 5432 "user" "password" "dbname"

runSession :: Connection -> Statement params result -> params -> IO result
runSession conn st params = do
    run (statement params st) conn >>= \case
        Left err  -> print err >> exitFailure
        Right res -> pure res

data Mode
    = InsertMode
    | SelectMode

insertMode :: Connection -> IO ()
insertMode conn = do
    tid1 <- runSession conn insertTest test1
    tid2 <- runSession conn insertTest test2
    print $ test1 { testKey = tid1 }
    print $ test2 { testKey = tid2 }
  where insertTest :: Statement Test (Key Test)
        insertTest = Statement sql (encode @Test) (decode @(Key Test)) False
          where sql = "INSERT INTO tests (age, description, author) \
                      \VALUES ($1, $2, $3) RETURNING id"

        test1 :: Test
        test1 = Test { testKey         = Key 0
                     , testAge         = 12
                     , testDescription = "fuck you"
                     , testAuthor      = Just "Van Darkholme"
                     , testUnused      = [1, 2, 3]
                     }

        test2 :: Test
        test2 = Test { testKey         = Key (-1)
                     , testAge         = 15
                     , testDescription = "Fucken slaves"
                     , testAuthor      = Nothing
                     , testUnused      = []
                     }

selectMode :: Connection -> IO ()
selectMode conn = runSession conn getTests () >>= mapM_ print
  where getTests :: Statement () [Test]
        getTests = Statement sql (encode @()) (decode @[Test]) False
        sql = "SELECT * FROM tests"

parseArgs :: IO Mode
parseArgs = do
    args <- getArgs
    when (length args /= 1) $ printHelp
    case args of
        ["select"] -> pure SelectMode
        ["insert"] -> pure InsertMode
        _          -> printHelp
  where printHelp = do
            prog <- getProgName
            putStr $ concat [ "Usage: " ++ prog ++ " <mode>\n\n"
                            , "mode can be one of:\n"
                            , "select\trun program in select mode\n"
                            , "insert\trun program in insert mode\n"
                            ]
            exitFailure

main :: IO ()
main = do
    mode <- parseArgs
    bracket acquireConn releaseConn $ \case
        Left err   -> BS.putStr $ showConnErr err
        Right conn -> case mode of
            SelectMode -> selectMode conn
            InsertMode -> insertMode conn
  where releaseConn = either (const $ pure ()) release
        acquireConn = acquire dbSettings
        showConnErr = \case
            Nothing  -> "Connection error\n"
            Just err -> "Connection error: " <> err <> "\n"
