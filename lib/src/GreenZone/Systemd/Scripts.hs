{-|
  GreenZone is working as a background process (aka daemon), so we're using
  @systemd@ for the management.
  These scripts are for convenience: start, stop and restart GreenZone.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module GreenZone.Systemd.Scripts
    ( startGreenZoneServiceSh
    , stopGreenZoneServiceSh
    , restartGreenZoneServiceSh
    , pathToStartGreenZoneServiceSh
    , pathToStopGreenZoneServiceSh
    , pathToRestartGreenZoneServiceSh
    ) where

import           Data.Monoid           ( (<>) )

import           Data.Text             ( Text, pack )
import           System.FilePath.Posix ( (<.>), (</>) )

import           GreenZone.Paths       ( baseName, pathToBinSubDirectory )

startGreenZoneServiceSh
  , stopGreenZoneServiceSh
  , restartGreenZoneServiceSh
    :: Text
startGreenZoneServiceSh   = pack $ shebang <> prefix <> "start "   <> baseName
stopGreenZoneServiceSh    = pack $ shebang <> prefix <> "stop "    <> baseName
restartGreenZoneServiceSh = pack $ shebang <> prefix <> "restart " <> baseName

pathToStartGreenZoneServiceSh
  , pathToStopGreenZoneServiceSh
  , pathToRestartGreenZoneServiceSh
    :: FilePath
pathToStartGreenZoneServiceSh   = pathToBinSubDirectory </> "start"   <> suffix
pathToStopGreenZoneServiceSh    = pathToBinSubDirectory </> "stop"    <> suffix
pathToRestartGreenZoneServiceSh = pathToBinSubDirectory </> "restart" <> suffix

shebang :: String
shebang = "#!/bin/sh\n"

prefix :: String
prefix = "sudo systemctl "

suffix :: FilePath
suffix = "-service" <.> "sh"
