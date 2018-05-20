{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module GreenZone.Loop.SBCSupervisor
    ( sbcSupervisorLoop
    ) where

import           Control.Monad                 ( forever, when )

import           Control.Exception.Safe        ( Exception, MonadThrow, throw )
import           Control.Monad.IO.Class        ( MonadIO )

import           GreenZone.Configuration.Types ( Configuration (..),
                                                 InternalConstants (..),
                                                 LoopDelay (..), Percentage (..),
                                                 TemperatureInC (..) )
import           GreenZone.Loop.Delay          ( sleepFor )
import           GreenZone.Tool.Shell          ( runShCommandWithResult )

-- | SBC critical problems.
data SBCProblem
    = BatteryDischarge Percentage
    | CPUOverheat TemperatureInC
    deriving Show

instance Exception SBCProblem

-- | SBC supervisor loop, it checks hardware critical problems.
sbcSupervisorLoop
    :: (MonadThrow m, MonadIO m)
    => Configuration
    -> m ()
sbcSupervisorLoop config = forever $ do
    energy <- getCurrentBatteryLevel
    when (energy < minBatteryLevel internalConfig) $
        throw (BatteryDischarge energy)

    temperature <- getCurrentCPUTemperature
    when (temperature > maxCPUTemperature internalConfig) $
        throw (CPUOverheat temperature)

    sleepFor delay
  where
    internalConfig = internal config
    delay = forSBC . loopDelay $ internalConfig

getCurrentBatteryLevel :: MonadIO m => m Percentage
getCurrentBatteryLevel = do
    _ <- runShCommandWithResult "acpi" ["-b"]
    -- Battery 0: Discharging, 73%, 02:50:20 remaining
    -- Battery 1: Full, 100%
    --
    return $ Percentage 10

getCurrentCPUTemperature :: MonadIO m => m TemperatureInC
getCurrentCPUTemperature = do
    _ <- runShCommandWithResult "acpi" ["-t"]
    -- Thermal 0: ok, 37.0 degrees C
    --
    return $ TemperatureInC 45
