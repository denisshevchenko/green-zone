{-# LANGUAGE Safe #-}

module GreenZone.Cache.GetPut
    ( putGlucoseEntry
    , getGlucoseEntry
    ) where

import           Control.Concurrent.STM ( atomically, readTVar, tryReadTQueue,
                                          writeTQueue, writeTVar )
import           Control.Monad.IO.Class ( MonadIO, liftIO )

import           GreenZone.Cache.Types  ( Cache (..), SharedCache )
import           GreenZone.Loop.Types   ( GlucoseEntry (..) )

-- | Stores blood glucose entry in the queue.
putGlucoseEntry
    :: MonadIO m
    => GlucoseEntry
    -> SharedCache
    -> m ()
putGlucoseEntry newGlucoseEntry cache = liftIO . atomically $ do
    currentCache <- readTVar cache
    writeTQueue (glucoseEntries currentCache) newGlucoseEntry
    writeTVar cache currentCache

-- | Reads the next blood glucose entry from the queue.
-- Returns 'Nothing' if there's no entries in the queue.
getGlucoseEntry
    :: MonadIO m
    => SharedCache
    -> m (Maybe GlucoseEntry)
getGlucoseEntry cache = liftIO . atomically $
    readTVar cache >>= tryReadTQueue . glucoseEntries
