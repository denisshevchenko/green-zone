{-|
  GreenZone is working as a background process (aka daemon), so we're using
  @systemd@ for the management.
-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module GreenZone.Systemd.Service where

import           Data.Monoid           ( (<>) )

import           Data.Text             ( Text, intercalate, pack )
import           System.FilePath.Posix ( (<.>), (</>) )

import           GreenZone.Paths       ( baseName, pathToConfigurationFile,
                                         pathToGreenZoneExe )

-- | Creates @systemd@ unit, for /etc/systemd/system directory.
createGreenZoneUnit :: Text
createGreenZoneUnit = intercalate "\n"
    [ "[Unit]"
    , "Description=GreenZone daemon"
    , ""
    , "[Service]"
    , "User=" <> user
    , "ExecStart=" <> daemon <> " " <> option
    , "Restart=on-failure"
    , ""
    , "[Install]"
    , "WantedBy=multi-user.target"
    ]
  where
    user   = pack baseName
    daemon = pack pathToGreenZoneExe
    option = pack pathToConfigurationFile

-- | Standard path where GreenZone unit will be stored.
pathToGreenZoneUnit :: FilePath
pathToGreenZoneUnit = "/etc/systemd/system" </> baseName <.> "service"
