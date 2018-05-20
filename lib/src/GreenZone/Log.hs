{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GreenZone.Log
    ( LogLevel (..)
    , LogName (..)
    , pathsToLogs
    , pathsToLogSubDirectories
    , toPumpLog
    , toCloudLog
    , toCGMLog
    , toSBCLog
    , toGlobalLog
    , toGlobalLogInit
    ) where

import           Data.Char                       ( toLower )
import           Data.Monoid                     ( (<>) )

import           Control.Monad.IO.Class          ( MonadIO, liftIO )
import           Data.Text                       ( Text, pack )
import qualified Data.Text.IO                    as TIO
import           Data.Time.Clock                 ( getCurrentTime )
import           Data.Time.Format                ( defaultTimeLocale,
                                                   formatTime )
import           System.Console.Pretty           ( Color (..), color )
import           System.FilePath.Posix           ( takeDirectory, (<.>), (</>) )

import           GreenZone.Paths                 ( pathToInstallDirectory )
import           GreenZone.Tool.IgnoreExceptions ( ignoreExceptions )
import           GreenZone.Tool.Text             ( showT )

-- | Log levels.
data LogLevel
    = INFO
    | WARNING
    | ERROR
    deriving Show

-- | It is convenient to collect logs in subdirectories.
data LogName
    = CGM
    | Cloud
    | Global
    | Pump
    | SBC
    deriving (Enum, Eq, Show)

-- | Each loop writes in the separate log, there're two reasons for it:
-- 1. It's convenient during logs analyzing.
-- 2. No write-collisions between threads.
toPumpLog
  , toCloudLog
  , toCGMLog
  , toSBCLog
  , toGlobalLog
    :: MonadIO m
    => LogLevel
    -> Text
    -> m ()
toPumpLog   level message = toLog level message $ pathToOneLog Pump
toCloudLog  level message = toLog level message $ pathToOneLog Cloud
toCGMLog    level message = toLog level message $ pathToOneLog CGM
toSBCLog    level message = toLog level message $ pathToOneLog SBC
toGlobalLog level message = toLog level message $ pathToOneLog Global

toGlobalLogInit :: MonadIO m => m ()
toGlobalLogInit =
    liftIO $ ignoreExceptions $ TIO.appendFile (pathToOneLog Global) "\nGREENZONE IS LAUNCHED\n"

toLog
    :: MonadIO m
    => LogLevel
    -> Text
    -> FilePath
    -> m ()
toLog logLevel message pathToLog = liftIO $ do
    timeMark <- formatTime defaultTimeLocale "%F %T %Z" <$> getCurrentTime
    let prefix       = "[" <> pack timeMark <> "] " <> showT logLevel <> ": "
        messageLn    = message <> "\n"
        wholeMessage = prefix <> messageLn
    ignoreExceptions $ TIO.appendFile pathToLog wholeMessage
    ignoreExceptions $ repeatToStdOut logLevel prefix messageLn

-- | Print the same message to terminal, in corresponding color.
repeatToStdOut
    :: MonadIO m
    => LogLevel
    -> Text
    -> Text
    -> m ()
repeatToStdOut INFO    prefix message = liftIO $ TIO.putStr (color Green  prefix) >> TIO.putStr message
repeatToStdOut WARNING prefix message = liftIO $ TIO.putStr (color Yellow prefix) >> TIO.putStr message
repeatToStdOut ERROR   prefix message = liftIO $ TIO.putStr (color Red    prefix) >> TIO.putStr message

-- | Full path to particular log.
pathToOneLog :: LogName -> FilePath
pathToOneLog logName =
    pathToInstallDirectory </> "logs" </> name </> name <.> "log"
  where
    name = toLower <$> show logName -- Unix-way, low case names for subdirs.

-- | Full paths to all logs.
pathsToLogs :: [FilePath]
pathsToLogs = pathToOneLog <$> [CGM .. SBC]

-- | Fille paths to all logs subdirectories.
pathsToLogSubDirectories :: [FilePath]
pathsToLogSubDirectories = takeDirectory <$> pathsToLogs
