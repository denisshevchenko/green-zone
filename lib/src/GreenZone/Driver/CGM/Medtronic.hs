{-# LANGUAGE Safe #-}

module GreenZone.Driver.CGM.Medtronic
    ( getCurrentGlucoseFromMMT7703
    ) where

import           Control.Monad.IO.Class        ( MonadIO )

import           GreenZone.Configuration.Types ( Configuration (..) )
import           GreenZone.Loop.Types          ( GlucoseEntry (..),
                                                 GlucoseEntryError (..) )

-- |
getCurrentGlucoseFromMMT7703
    :: MonadIO m
    => Configuration
    -> m (Either GlucoseEntryError GlucoseEntry)
getCurrentGlucoseFromMMT7703 _ = do
    -- 1. Prepare low-level command.
    -- 2. Send it to tty-port, for radio stick.
    -- 3. Process response.
    return $ Right $ GlucoseEntry 1
