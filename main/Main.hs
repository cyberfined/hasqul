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
import Database.Hasqul.Updater
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

data TestUpdate = TestUpdate
    { updAge         :: !(Maybe Int32)
    , updDescription :: !(Maybe Text)
    , updAuthor      :: !(Maybe (Maybe Text))
    } deriving stock Generic
      deriving Updatable via (Updater '[TableName "tests", StripPrefix "upd"] TestUpdate)

dbSettings :: Settings
dbSettings = settings "192.168.0.102" 5432 "undefined" "password" "backenddb"

runSession :: Connection -> Session result -> IO result
runSession conn s = run s conn >>= \case
    Left err  -> print err >> exitFailure
    Right res -> pure res

runStatement :: Connection -> Statement params result -> params -> IO result
runStatement conn st params = runSession conn (statement params st)

data Mode
    = InsertMode
    | SelectMode
    | UpdateMode

insertTest :: Statement Test (Key Test)
insertTest = Statement sql (encode @Test) (decode @(Key Test)) False
  where sql = "INSERT INTO tests (age, description, author) \
              \VALUES ($1, $2, $3) RETURNING id"

insertMode :: Connection -> IO ()
insertMode conn = do
    tid1 <- runStatement conn insertTest test1
    tid2 <- runStatement conn insertTest test2
    print $ test1 { testKey = tid1 }
    print $ test2 { testKey = tid2 }
  where test1 :: Test
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
selectMode conn = runStatement conn getTests () >>= mapM_ print
  where getTests :: Statement () [Test]
        getTests = Statement sql (encode @()) (decode @[Test]) False
        sql = "SELECT * FROM tests"

updateMode :: Connection -> IO ()
updateMode conn = runSession conn $ do
    Key tid <- statement test insertTest
    update (Key tid) tUpdate
  where test :: Test
        test = Test { testKey         = Key 0
                    , testAge         = 12
                    , testDescription = "Big surprise"
                    , testAuthor      = Just "Billy Herrington"
                    , testUnused      = []
                    }

        tUpdate :: TestUpdate
        tUpdate = TestUpdate { updAge         = Just 15
                             , updDescription = Nothing
                             , updAuthor      = Just Nothing
                             }

parseArgs :: IO Mode
parseArgs = do
    args <- getArgs
    when (length args /= 1) $ printHelp
    case args of
        ["select"] -> pure SelectMode
        ["insert"] -> pure InsertMode
        ["update"] -> pure UpdateMode
        _          -> printHelp
  where printHelp = do
            prog <- getProgName
            putStr $ concat [ "Usage: " ++ prog ++ " <mode>\n\n"
                            , "mode can be one of:\n"
                            , "select\trun program in select mode\n"
                            , "insert\trun program in insert mode\n"
                            , "update\trun program in update mode\n"
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
            UpdateMode -> updateMode conn
  where releaseConn = either (const $ pure ()) release
        acquireConn = acquire dbSettings
        showConnErr = \case
            Nothing  -> "Connection error\n"
            Just err -> "Connection error: " <> err <> "\n"
