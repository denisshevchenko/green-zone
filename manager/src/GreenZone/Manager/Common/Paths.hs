{-# LANGUAGE Trustworthy #-}

module GreenZone.Manager.Common.Paths
    ( pathToInstallDirectory
    , pathToGreenZoneExe
    , pathsToSubDirectories
    ) where

import           Prelude         hiding ( FilePath )

import           Data.Text       ( pack )
import           Shelly          ( FilePath, Sh, fromText )

import qualified GreenZone.Log   as P
import qualified GreenZone.Paths as P

pathToInstallDirectory
  , pathToGreenZoneExe
    :: Sh FilePath
pathToInstallDirectory = return $ toShellyPath P.pathToInstallDirectory
pathToGreenZoneExe     = return $ toShellyPath P.pathToGreenZoneExe

pathsToSubDirectories :: Sh [FilePath]
pathsToSubDirectories = do
    let allSubDirs = [ P.pathToBinSubDirectory
                     , P.pathToConfSubDirectory
                     , P.pathToCacheSubDirectory
                     ] ++ P.pathsToLogSubDirectories
    return $ toShellyPath <$> allSubDirs

toShellyPath :: String -> FilePath
toShellyPath = fromText . pack
