{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}

module GreenZone.Log.Archivist
    ( logArchivistLoop
    ) where

import           Control.Monad                   ( forever, mapM, mapM_, when )
import           Data.List                       ( minimum )
import           Data.Monoid                     ( mempty, (<>) )
import           Data.Word                       ( Word8 )

import           Control.Exception.Safe          ( MonadCatch )
import           Control.Monad.IO.Class          ( MonadIO, liftIO )
import           Data.Text                       ( pack )
import           Data.Time.Clock                 ( getCurrentTime )
import           Data.Time.Format                ( defaultTimeLocale,
                                                   formatTime )
import           Shelly                          ( errExit, run_, shelly )
import           System.Directory                ( doesFileExist, getFileSize,
                                                   listDirectory,
                                                   removePathForcibly,
                                                   renamePath )
import           System.FilePath.Posix           ( replaceFileName,
                                                   takeBaseName, takeDirectory,
                                                   (<.>) )

import           GreenZone.Configuration.Types   ( Configuration (..),
                                                   InternalConstants (..),
                                                   Logging (..),
                                                   SizeInBytes (..) )
import           GreenZone.Log                   ( pathsToLogs )
import           GreenZone.Loop.Delay            ( sleepFor )
import           GreenZone.Tool.IgnoreExceptions ( ignoreExceptions )

-- | Log archivist loop checks all logs, and, if particular log
-- is too big, it archives it (and periodically rotates these archives).
logArchivistLoop
    :: (MonadIO m, MonadCatch m)
    => Configuration
    -> m ()
logArchivistLoop config = forever $ do
    pathsToBigLogs <- filter (not . null) <$> mapM (checkIfLogIsBig maxSize) pathsToLogs
    ignoreExceptions $ mapM_ (archiveLog maxNumber) pathsToBigLogs
    sleepFor delay
  where
    delay     = archivistDelay  logConfig
    maxSize   = maxLogSize      logConfig
    maxNumber = maxNumberOfLogs logConfig
    logConfig = logging . internal $ config

-- | Checks if the log is already too big.
checkIfLogIsBig
    :: MonadIO m
    => SizeInBytes
    -> FilePath
    -> m FilePath
checkIfLogIsBig (SizeInBytes maxSizeOfLog) pathToLog = liftIO $ do
    logIsHere <- doesFileExist pathToLog
    if logIsHere
        then do
            sizeInBytes <- getFileSize pathToLog
            if sizeInBytes > fromIntegral maxSizeOfLog
                then return pathToLog
                else return mempty
        else return mempty

-- | Archives a big log as ".tar.bz2". If there's a lot of archives -
-- removes the oldest one.
archiveLog
    :: MonadIO m
    => Word8
    -> FilePath
    -> m ()
archiveLog maximalNumberOfLogs pathToLog = liftIO $ do
    let pathToTempLog = pathToLog <.> "tmp"
    renamePath pathToLog pathToTempLog
    dateAndTime <- formatTime defaultTimeLocale "%FT%H-%M-%S" <$> getCurrentTime
    let logName       = takeBaseName pathToTempLog
        archiveName   = dateAndTime <> "_" <> logName <.> "tar.bz2"
        pathToArchive = replaceFileName pathToLog archiveName
    -- Any Linux definitely contains "tar" command.
    shelly $ errExit False $ run_ "tar" ["cfj", pack pathToArchive, pack pathToTempLog]
    removePathForcibly pathToTempLog
    -- Check if there's a lot of archives...
    logAndArchives <- listDirectory $ takeDirectory pathToLog
    when (length logAndArchives > fromIntegral maximalNumberOfLogs) $ do
        -- Archives contains date in their names, so the oldest archive will
        -- always be the "minimal" one.
        let oldestArchive = minimum logAndArchives
        removePathForcibly oldestArchive
