{-# LANGUAGE Safe         #-}
{-# LANGUAGE ViewPatterns #-}

module GreenZone.Loop.Delay where

import           Control.Concurrent            ( threadDelay )

import           Control.Monad.IO.Class        ( MonadIO, liftIO )

import           GreenZone.Configuration.Types ( DelayInMinutes (..) )

-- | Suspends current thread.
sleepFor :: MonadIO m => DelayInMinutes -> m ()
sleepFor (DelayInMinutes (fromIntegral -> mins)) = liftIO $
    threadDelay $ microsecsInSec * secsInMin * mins
  where
    microsecsInSec = 1000000
    secsInMin      = 60
