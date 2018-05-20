{-# LANGUAGE Safe #-}

module GreenZone.Paths where

import           System.FilePath.Posix ( (<.>), (</>) )

-- | It is assumed that just one copy of GreenZone works on SBC.
-- So there's no need to ask user to choose installation directory,
-- we just use user's home directory and @baseName@ subdirectory.
pathToInstallDirectory
  , pathToBinSubDirectory
  , pathToConfSubDirectory
  , pathToGreenZoneExe
  , pathToConfigurationFile
  , pathToCacheSubDirectory
  , pathToCacheFile
    :: FilePath
pathToInstallDirectory  = "/opt" </> baseName
pathToBinSubDirectory   = pathToInstallDirectory  </> "bin"
pathToConfSubDirectory  = pathToInstallDirectory  </> "conf"
pathToGreenZoneExe      = pathToBinSubDirectory   </> baseName
pathToConfigurationFile = pathToConfSubDirectory  </> baseName <.> "yaml"
pathToCacheSubDirectory = pathToInstallDirectory  </> "cache"
pathToCacheFile         = pathToCacheSubDirectory </> "cache"

baseName :: FilePath
baseName = "green-zone"
