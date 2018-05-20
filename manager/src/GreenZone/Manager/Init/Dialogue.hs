{-|
Module      : GreenZone.Manager.Init.Dialogue
Description : Init dialog.
Portability : POSIX

This module defines init questions:
    1. Choose a language to be used during installation.
    2. Choose an action that Manager should perform.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

module GreenZone.Manager.Init.Dialogue
    ( chooseALanguage
    , chooseAnAction
    ) where

import           Data.List                              ( lookup )
import           Data.Maybe                             ( maybe )
import           Data.Version                           ( showVersion )

import           Control.Monad.IO.Class                 ( MonadIO )
import qualified Data.Text                              as T
import           Shelly

import           GreenZone.Configuration.Default        ()
import           GreenZone.Manager.Common.Boxes         ( chooseOneItem )
import           GreenZone.Manager.Common.DialogueTools ( mkItem )
import           GreenZone.Manager.Common.Localization
import           GreenZone.Manager.Common.Paths         ( pathToGreenZoneExe,
                                                          pathToInstallDirectory )
import           GreenZone.Manager.Common.Types
import           GreenZone.Tool.Text                    ( readT )
import           Paths_green_zone                       ( version )

-- | User has to choose language to be used for the installation.
chooseALanguage
    :: MonadIO m
    => m Localization
chooseALanguage = shelly $ do
    initialLang <- toOurLocale <$> get_env_text "LANG"
    whatLanguage <$> chooseOneItem initialLang
                                   (BoxSize (Height 15) (Width 60))
                                   (welcomeTitle initialLang)
                                   (welcomeMessage initialLang)
                                   (okButton initialLang)
                                   (cancelButton initialLang)
                                   langItems
  where
    toOurLocale rawLocale = maybe defaultLang fst $ lookup code langCodes
      where code = T.take 2 rawLocale -- For example, "en" from "en_US.UTF-8".
    langItems = [mkItem locale label | (_, (locale, label)) <- langCodes]
    whatLanguage (Tag itemTag) = readT itemTag :: Localization

-- | User has to choose an action for Manager.
chooseAnAction
    :: MonadIO m
    => Localization
    -> m ManagerAction
chooseAnAction lang = shelly $ do
    itIsAlreadyHere <- checkIfGreenZoneIsInstalled
    if itIsAlreadyHere
        then do
            itIsCompatibleVersion <- checkBackwardCompatibility
            if itIsCompatibleVersion
                then whatUserWants <$> chooseOneItem lang
                                                     (BoxSize (Height 15) (Width 60))
                                                     (welcomeTitle lang)
                                                     (welcomeMessage lang)
                                                     (okButton lang)
                                                     (cancelButton lang)
                                                     [ mkItem ReConfigureGreenZone $ reConfigureLabel lang
                                                     , mkItem InstallGreenZone     $ installLabel lang
                                                     , mkItem DeleteGreenZone      $ deleteLabel lang
                                                     ]
                else whatUserWants <$> chooseOneItem lang
                                                     (BoxSize (Height 15) (Width 70))
                                                     (welcomeTitle lang)
                                                     (welcomeMessage lang)
                                                     (okButton lang)
                                                     (cancelButton lang)
                                                     [ mkItem InstallGreenZone $ installLabel lang
                                                     , mkItem DeleteGreenZone  $ deleteLabel lang
                                                     ]
        else return InstallGreenZone
  where
    whatUserWants (Tag itemTag) = readT itemTag :: ManagerAction

-- | Checks if GreenZone is already installed on this SBC.
checkIfGreenZoneIsInstalled :: Sh Bool
checkIfGreenZoneIsInstalled = do
    installDirectoryExists <- test_d =<< pathToInstallDirectory
    greenZoneExeIsHere     <- test_f =<< pathToGreenZoneExe
    return $ installDirectoryExists && greenZoneExeIsHere

-- | Checks if this GreenZone is backward compatible with installed one:
-- 1. Ask installed @green-zone@ its version.
-- 2. Compare it with current version.
-- 3. If they are equal - this GreenZone is compatible with
--    installed one. It means that it is possible to re-configure
--    installed GreenZone using this one.
-- 4. If not - this GreenZone is incompatible with installed one.
--    It means that installed version is too old, so this GreenZone
--    cannot be used for re-configuration of installed one.
checkBackwardCompatibility :: Sh Bool
checkBackwardCompatibility = do
    installedExe <- pathToGreenZoneExe
    installedExeVersion <- run installedExe ["-v"]
    let currentVersion = T.pack $ showVersion version
    return $ installedExeVersion == currentVersion
