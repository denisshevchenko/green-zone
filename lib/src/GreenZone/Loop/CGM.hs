{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module GreenZone.Loop.CGM
    ( cgmLoop
    ) where

import           Control.Monad                 ( forever )
import           Data.Monoid                   ( (<>) )

import           Control.Monad.IO.Class        ( MonadIO )

import           GreenZone.Cache.GetPut        ( putGlucoseEntry )
import           GreenZone.Cache.Types         ( SharedCache )
import           GreenZone.Command.Glucose     ( getCurrentGlucose )
import           GreenZone.Configuration.Types ( Configuration (..),
                                                 InternalConstants (..),
                                                 LoopDelay (..) )
import           GreenZone.Log                 ( LogLevel (..), toCGMLog )
import           GreenZone.Loop.Delay          ( sleepFor )
import           GreenZone.Tool.Text           ( showT )

-- | CGM loop. Asks current value of the blood glucose and stores it
-- in the queue.
cgmLoop
    :: (MonadIO m)
    => Configuration
    -> SharedCache
    -> m ()
cgmLoop config cache = forever $ do
    getCurrentGlucose config >>= \case
        Left problem  -> toCGMLog ERROR $ "CGM loop, unable to get current glucose: "
                                          <> showT problem
        Right glucose -> putGlucoseEntry glucose cache
    sleepFor delay
  where
    delay = forCGM . loopDelay . internal $ config
