{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module GreenZone.Loop.Pump
    ( pumpLoop
    ) where

import           Control.Monad                 ( forever )
-- import           Data.Monoid                   ( (<>) )

import           Control.Monad.IO.Class        ( MonadIO )

import           GreenZone.Cache.Types         ( SharedCache )
import           GreenZone.Configuration.Types ( Configuration (..),
                                                 InternalConstants (..),
                                                 LoopDelay (..) )
-- import           GreenZone.Log                 ( LogLevel (..), toPumpLog )
import           GreenZone.Loop.Delay          ( sleepFor )

-- | Pump loop. Asks current value of the blood glucose and stores it
-- in the queue.
pumpLoop
    :: (MonadIO m)
    => Configuration
    -> SharedCache
    -> m ()
pumpLoop config _ = forever $ do
    sleepFor delay
  where
    delay = forPump . loopDelay . internal $ config
