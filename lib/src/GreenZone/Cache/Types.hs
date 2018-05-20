{-# LANGUAGE Safe #-}

module GreenZone.Cache.Types where

import           Control.Concurrent.STM ( TQueue, TVar )

import           GreenZone.Loop.Types   ( GlucoseEntry (..) )

-- | Structure that will be shared among loops. Contains queues (classic
-- FIFO-structures), for sequential work with the data.
data Cache = Cache
    { glucoseEntries :: TQueue GlucoseEntry
    , b              :: TQueue String
    }

-- | Transactional variable, for sharing among loops.
type SharedCache = TVar Cache
