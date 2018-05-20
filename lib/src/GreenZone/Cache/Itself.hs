{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GreenZone.Cache.Itself
    ( writeCacheToFile
    , readCacheFromFile
    ) where

import           Control.Monad          ( mapM_ )
import           Data.Monoid            ( (<>) )
import           GHC.Generics           ( Generic )

import           Control.Concurrent.STM ( atomically, flushTQueue, newTQueue,
                                          newTVar, readTVar, writeTQueue )
import           Control.Exception.Safe ( SomeException, try )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Binary            ( Binary, decodeFileOrFail, encodeFile )
import           Data.Text              ( pack )

import           GreenZone.Cache.Types  ( Cache (..), SharedCache )
import           GreenZone.Log          ( LogLevel (..), toGlobalLog )
import           GreenZone.Loop.Types   ( GlucoseEntry (..) )
import           GreenZone.Paths        ( pathToCacheFile )
import           GreenZone.Tool.Text    ( showT )

-- |
writeCacheToFile :: MonadIO m => SharedCache -> m ()
writeCacheToFile cache = liftIO $ do
    currentCache <- atomically $ readTVar cache
    serializable <- toSerializable currentCache
    try (encodeFile pathToCacheFile serializable) >>= \case
        Left (problem :: SomeException) ->
            toGlobalLog WARNING $ "Unable to encode cache: " <> showT problem
                                  <> ". Operation is skipped."
        Right _ ->
            return ()

-- |
readCacheFromFile :: MonadIO m => m SharedCache
readCacheFromFile = liftIO $
    decodeFileOrFail pathToCacheFile >>= \case
        Left (_, problem) -> do
            toGlobalLog WARNING $ "Unable to read cache: " <> pack problem
                                  <> ". New cache will be created."
            newSharedCache
        Right serializable -> do
            toGlobalLog INFO "Cache read successfully."
            atomically . newTVar =<< toCache serializable

-- |
newSharedCache :: MonadIO m => m SharedCache
newSharedCache = liftIO . atomically . newTVar =<< emptyCache

-- | Creates empty 'Cache'.
emptyCache :: MonadIO m => m Cache
emptyCache = liftIO $ Cache
    <$> atomically newTQueue
    <*> atomically newTQueue

-------------------------------------------------------------------------------
-- Serializable cache.
-------------------------------------------------------------------------------

-- | It's impossible to serialize a structure with 'TQueue',
-- so make the structure with lists instead.
data SerializableCache = SerializableCache
    { serGlucoseEntries :: [GlucoseEntry]
    } deriving Generic

instance Binary GlucoseEntry
instance Binary SerializableCache

-- | Creates 'SerializableCache' from 'Cache'.
toSerializable :: MonadIO m => Cache -> m SerializableCache
toSerializable cache = liftIO . atomically $ do
    serGlucoseEntries <- flushTQueue . glucoseEntries $ cache
    return SerializableCache {..}

-- | Creates 'Cache' from 'SerializableCache'.
toCache :: MonadIO m => SerializableCache -> m Cache
toCache serializable = do
    cache <- emptyCache
    fillQueue (glucoseEntries cache) (serGlucoseEntries serializable)
    return cache
  where
    fillQueue queue list = liftIO . atomically $
        mapM_ (writeTQueue queue) list
