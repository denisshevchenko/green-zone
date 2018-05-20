{-# LANGUAGE Safe #-}

module GreenZone.Command.Glucose
    ( getCurrentGlucose
    ) where

import           Control.Monad.IO.Class         ( MonadIO )

import           GreenZone.Configuration.Types  ( CGMConfiguration (..),
                                                  CGMModel (..),
                                                  Configuration (..) )
import           GreenZone.Driver.CGM.Medtronic ( getCurrentGlucoseFromMMT7703 )
import           GreenZone.Loop.Types           ( GlucoseEntry (..),
                                                  GlucoseEntryError (..) )

-- | Tries to get current blood glucose data. And settings from 'Configuration'
-- will be used for actual getting of data (e.g CGM model).
getCurrentGlucose
    :: MonadIO m
    => Configuration
    -> m (Either GlucoseEntryError GlucoseEntry)
getCurrentGlucose config =
    case cgmModel . cgm $ config of
        MedtronicMMT7703 -> getCurrentGlucoseFromMMT7703 config
