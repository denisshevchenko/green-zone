{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module GreenZone.Configuration.Default
    ( defaultConfiguration
    ) where

import           GreenZone.Configuration.Types

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
    { nightScout       = defaultNightScoutConfiguration
    , pump             = defaultPumpConfiguration
    , cgm              = defaultCGMConfiguration
    , ttyPort          = mempty
    , maxIOB           = 0
    , autoSenseEnabled = False
    , autoTuneEnabled  = False
    , sudoPassword     = mempty
    , internal         = defaultInternalConstants
    }

defaultNightScoutConfiguration :: NightScoutConfiguration
defaultNightScoutConfiguration = NightScoutConfiguration
    { host      = ""
    , apiSecret = ""
    }

defaultLoopDelay :: LoopDelay
defaultLoopDelay = LoopDelay
    { forCGM   = DelayInMinutes 5
    , forCloud = DelayInMinutes 5
    , forPump  = DelayInMinutes 5
    , forSBC   = DelayInMinutes 5
    }

defaultLogging :: Logging
defaultLogging = Logging
    { archivistDelay  = DelayInMinutes 50
    , maxLogSize      = SizeInBytes $ 1024 * 1024 * 5
    , maxNumberOfLogs = 6
    }

defaultInternalConstants :: InternalConstants
defaultInternalConstants = InternalConstants
    { maxCPUTemperature = TemperatureInC 85.0
    , minBatteryLevel   = Percentage 3
    , loopDelay         = defaultLoopDelay
    , logging           = defaultLogging
    }

defaultPumpConfiguration :: PumpConfiguration
defaultPumpConfiguration = PumpConfiguration
    { pumpModel    = MedtronicMMT722
    , serialNumber = ""
    , worldRegion  = WW
    }

defaultCGMConfiguration :: CGMConfiguration
defaultCGMConfiguration = CGMConfiguration
    { cgmModel = MedtronicMMT7703
    }
