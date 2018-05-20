{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Trustworthy   #-}

module GreenZone.Configuration.Types where

import           Data.Word    ( Word32, Word8 )
import           GHC.Generics ( Generic )

import           Data.Text    ( Text )
import           Data.Yaml    ( FromJSON, ToJSON )

-- | Temperature, in Celcius degrees, for CPU overheat check.
newtype TemperatureInC = TemperatureInC Double
    deriving (Eq, Generic, Ord, Show)

-- | Percentage, for battery level.
newtype Percentage = Percentage Word8
    deriving (Eq, Generic, Ord, Show)

-- | Delay for loops, in minutes.
newtype DelayInMinutes = DelayInMinutes Word8
    deriving (Generic, Show)

-- | Size of the file, in bytes.
newtype SizeInBytes = SizeInBytes Word32
    deriving (Generic, Show)

-- | NightScout configuration, part of complete configuration.
data NightScoutConfiguration = NightScoutConfiguration
    { host      :: !Text
    -- ^ NightScout application's complete host.
    , apiSecret :: !Text
    -- ^ NightScout API secret for authentication.
    } deriving (Generic, Show)

-- | Each loop works with some "period delay".
data LoopDelay = LoopDelay
    { forCGM   :: !DelayInMinutes
    , forCloud :: !DelayInMinutes
    , forPump  :: !DelayInMinutes
    , forSBC   :: !DelayInMinutes
    } deriving (Generic, Show)

-- | Logging settings.
data Logging = Logging
    { archivistDelay  :: !DelayInMinutes
    , maxLogSize      :: !SizeInBytes
    , maxNumberOfLogs :: !Word8
    } deriving (Generic, Show)

-- | Internal constants. The user won't be asked about these values during
-- GreenZone installation. And usually user shouldn't change these values.
data InternalConstants = InternalConstants
    { maxCPUTemperature :: !TemperatureInC
    , minBatteryLevel   :: !Percentage
    , loopDelay         :: !LoopDelay
    , logging           :: !Logging
    } deriving (Generic, Show)

data CGMModel
    = MedtronicMMT7703
    deriving (Eq, Generic, Ord, Show)

data CGMConfiguration = CGMConfiguration
    { cgmModel :: !CGMModel
    } deriving (Generic, Show)

-- | For Medtronic pumps there're two regions (radio frequency depends on it).
data WorldRegion
    = NA -- ^ North America.
    | WW -- ^ Worldwide.
    deriving (Generic, Show)

data PumpModel
    = MedtronicMMT722
    deriving (Generic, Show)

data PumpConfiguration = PumpConfiguration
    { pumpModel    :: !PumpModel
    , serialNumber :: !Text
    , worldRegion  :: !WorldRegion
    } deriving (Generic, Show)

-- | Complete configuration for GreenZone program.
data Configuration = Configuration
    { nightScout       :: !NightScoutConfiguration
    -- ^ NightScout configuration.
    , pump             :: !PumpConfiguration
    -- ^ Pump configuration.
    , cgm              :: !CGMConfiguration
    -- ^ CGM configuration.
    , ttyPort          :: !Text
    -- ^ TTY-port used to communicate with pump/CGM via stick.
    , maxIOB           :: !Word8
    -- ^ Max IOB value.
    , autoSenseEnabled :: !Bool
    -- ^ If 'True' - enable autosense.
    , autoTuneEnabled  :: !Bool
    -- ^ If 'True' - enable autotune.
    , sudoPassword     :: !Text
    -- ^ @sudo@-password. It is assumed that user is already in sudoers.
    , internal         :: !InternalConstants
    -- ^ Internal constants (usually user shouldn't change it).
    } deriving (Generic, Show)

-------------------------------------------------------------------------------
-- Yaml-instances. These instances provide an ability to encode values to Yaml
-- and decode them from Yaml automatically.
-------------------------------------------------------------------------------

instance ToJSON   Percentage
instance ToJSON   TemperatureInC
instance ToJSON   DelayInMinutes
instance ToJSON   SizeInBytes
instance ToJSON   NightScoutConfiguration
instance ToJSON   LoopDelay
instance ToJSON   Logging
instance ToJSON   InternalConstants
instance ToJSON   CGMModel
instance ToJSON   CGMConfiguration
instance ToJSON   PumpModel
instance ToJSON   PumpConfiguration
instance ToJSON   WorldRegion
instance ToJSON   Configuration

instance FromJSON Percentage
instance FromJSON TemperatureInC
instance FromJSON DelayInMinutes
instance FromJSON SizeInBytes
instance FromJSON NightScoutConfiguration
instance FromJSON LoopDelay
instance FromJSON Logging
instance FromJSON InternalConstants
instance FromJSON CGMModel
instance FromJSON CGMConfiguration
instance FromJSON PumpModel
instance FromJSON PumpConfiguration
instance FromJSON WorldRegion
instance FromJSON Configuration
