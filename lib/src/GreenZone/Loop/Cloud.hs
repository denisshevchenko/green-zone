{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module GreenZone.Loop.Cloud
    ( cloudLoop
    ) where

import           Control.Monad                 ( forever )
-- import           Data.Monoid                   ( (<>) )

import           Control.Monad.IO.Class        ( MonadIO )

import           GreenZone.Cache.Types         ( SharedCache )
import           GreenZone.Configuration.Types ( Configuration (..),
                                                 InternalConstants (..),
                                                 LoopDelay (..) )
-- import           GreenZone.Log                 ( LogLevel (..), toCloudLog )
import           GreenZone.Loop.Delay          ( sleepFor )

-- | Cloud loop. Asks current value of the blood glucose and stores it
-- in the queue.
cloudLoop
    :: (MonadIO m)
    => Configuration
    -> SharedCache
    -> m ()
cloudLoop config _ = forever $ do
    sleepFor delay
  where
    delay = forCloud . loopDelay . internal $ config
