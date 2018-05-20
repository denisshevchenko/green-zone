{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

module GreenZone.Manager.Install.Dialogue
    ( startInstallDialogue
    ) where

import           Data.Word                              ( Word8 )
import           Prelude                                hiding ( FilePath )

import           Control.Monad.Trans.Class              ( lift )
import           Control.Monad.Trans.RWS.Strict         ( RWST, ask, evalRWST,
                                                          get, modify, tell )
import           Data.Text                              ( Text )
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as TIO
import           Shelly                                 ( Sh, mkdir, mkdir_p,
                                                          rm_rf, shelly, test_d,
                                                          when )

import           GreenZone.Configuration.Default        ( defaultConfiguration )
import           GreenZone.Configuration.File           ( storeConfiguration )
import           GreenZone.Configuration.Types          ( Configuration (..), NightScoutConfiguration (..),
                                                          PumpConfiguration (..) )
import           GreenZone.Configuration.Validators     ( maxIOBValidator, nightScoutAPISecretValidator,
                                                          nightScoutHostValidator,
                                                          pumpSerialNumberValidator )
import           GreenZone.Manager.Common.Boxes         ( chooseOneItem,
                                                          getUserInput,
                                                          getUserPassword,
                                                          showMessage )
import           GreenZone.Manager.Common.DialogueTools ( mkItem )
import           GreenZone.Manager.Common.Localization  ( cancelButton,
                                                          iDontWantToContinue,
                                                          iUnderstand, okButton )
import qualified GreenZone.Manager.Common.Localization  as Locale
import           GreenZone.Manager.Common.Paths         ( pathToInstallDirectory,
                                                          pathsToSubDirectories )
import           GreenZone.Manager.Common.Types         ( BoxSize (..),
                                                          Height (..), Item (..),
                                                          Localization (..),
                                                          NoButton (..),
                                                          Tag (..), Width (..),
                                                          YesButton (..) )
import           GreenZone.Tool.Text                    ( readT )

-- | Manager functions live in this context to have an access to the
-- language installation, log and whiteboard.
type ManagerEnv = RWST Localization Text Configuration Sh ()

startInstallDialogue :: Localization -> IO ()
startInstallDialogue lang = do
    ((), installationLog) <- shelly $ evalRWST installationSteps lang defaultConfiguration
    TIO.putStrLn installationLog
  where
    installationSteps =
           showStartMessage
        >> checkPreviousGreenZone
        >> notifyAboutSupportedPump     -- Currently only one pump is supported.
        >> notifyAboutSupportedCGM      -- Currently only one CGM is supported.
        >> notifyAboutExplorerBoard     -- Currently only Explorer Board is supported for radiolink.
        >> getPumpSerialNumber
        >> getNightScoutHost
        >> getNightScoutAPISecret
        >> getMaxIOB
        >> autoSenseEnableOrNot
        >> autoTuneEnableOrNot
        >> prepareInstallDirectory
        >> writeConfigurationFile
        -- >> showFinalMessage

showStartMessage :: ManagerEnv
showStartMessage = do
    lang <- ask
    lift $ showMessage lang
                       (BoxSize (Height 15) (Width 70))
                       (Locale.letSGetStartedTitle lang)
                       (Locale.letSGetStartedMessage lang)
                       (YesButton (iUnderstand lang))
                       (NoButton (iDontWantToContinue lang))

checkPreviousGreenZone :: ManagerEnv
checkPreviousGreenZone = do
    greenZoneDir <- lift pathToInstallDirectory
    greenZoneIsHere <- lift $ test_d greenZoneDir
    when greenZoneIsHere $ do
        lang <- ask
        lift $ showMessage lang
                           (BoxSize (Height 15) (Width 70))
                           (Locale.greenZoneIsHereTitle lang)
                           (Locale.greenZoneIsHereMessage lang)
                           (YesButton (iUnderstand lang))
                           (NoButton (iDontWantToContinue lang))
        -- User confirmed that installation has to continue...
        lift $ rm_rf greenZoneDir

notifyAboutSupportedPump :: ManagerEnv
notifyAboutSupportedPump = do
    lang <- ask
    lift $ showMessage lang
                       (BoxSize (Height 15) (Width 70))
                       (Locale.notificationAboutPumpTitle lang)
                       (Locale.notificationAboutPumpMessage lang)
                       (YesButton (iUnderstand lang))
                       (NoButton (iDontWantToContinue lang))

notifyAboutSupportedCGM :: ManagerEnv
notifyAboutSupportedCGM = do
    lang <- ask
    lift $ showMessage lang
                       (BoxSize (Height 15) (Width 70))
                       (Locale.notificationAboutCGMTitle lang)
                       (Locale.notificationAboutCGMMessage lang)
                       (YesButton (iUnderstand lang))
                       (NoButton (iDontWantToContinue lang))

notifyAboutExplorerBoard :: ManagerEnv
notifyAboutExplorerBoard = do
    lang <- ask
    lift $ showMessage lang
                       (BoxSize (Height 15) (Width 70))
                       (Locale.notificationAboutSBCTitle lang)
                       (Locale.notificationAboutSBCMessage lang)
                       (YesButton (iUnderstand lang))
                       (NoButton (iDontWantToContinue lang))
    -- Value specific for 900 MHz Explorer Board.
    modify $ \conf -> conf { ttyPort = "/dev/spidev5.1" }

getPumpSerialNumber :: ManagerEnv
getPumpSerialNumber = do
    lang <- ask
    number <- lift $ getUserInput lang
                                  (BoxSize (Height 15) (Width 70))
                                  (Locale.pumpSerialNumberTitle lang)
                                  (Locale.pumpSerialNumberMessage lang)
                                  (okButton lang)
                                  (cancelButton lang)
                                  pumpSerialNumberValidator
                                  (Locale.invalidPumpSerialNumberMessage lang)
    modify $ \conf ->
        let updatedConfiguration = (pump conf) { serialNumber = T.strip number }
        in conf { pump = updatedConfiguration }

getNightScoutHost :: ManagerEnv
getNightScoutHost = do
    lang <- ask
    host' <- lift $ getUserInput lang
                                 (BoxSize (Height 15) (Width 70))
                                 (Locale.nightScoutHostTitle lang)
                                 (Locale.nightScoutHostMessage lang)
                                 (okButton lang)
                                 (cancelButton lang)
                                 nightScoutHostValidator
                                 (Locale.invalidNightScoutHostMessage lang)
    modify $ \conf ->
        let updatedConfiguration = (nightScout conf) { host = T.strip host' }
        in conf { nightScout = updatedConfiguration }

getNightScoutAPISecret :: ManagerEnv
getNightScoutAPISecret = do
    lang <- ask
    apiSecret' <- lift $ getUserPassword lang
                                         (BoxSize (Height 15) (Width 70))
                                         (Locale.nightScoutAPISecretTitle lang)
                                         (Locale.nightScoutAPISecretMessage lang)
                                         (okButton lang)
                                         (cancelButton lang)
                                         nightScoutAPISecretValidator
                                         (Locale.invalidNightScoutAPISecretMessage lang)
    modify $ \conf ->
        let nightScoutConfiguration = nightScout conf
            updatedConfiguration    = nightScoutConfiguration { apiSecret = T.strip apiSecret' }
        in conf { nightScout = updatedConfiguration }

getMaxIOB :: ManagerEnv
getMaxIOB = do
    lang <- ask
    iob <- lift $ getUserInput lang
                               (BoxSize (Height 15) (Width 70))
                               (Locale.maxIOBTitle lang)
                               (Locale.maxIOBMessage lang)
                               (okButton lang)
                               (cancelButton lang)
                               maxIOBValidator
                               (Locale.invalidMaxIOBMessage lang)
    -- It is safe because of validator.
    let iobAsNumber = read (T.unpack . T.strip $ iob) :: Word8
    modify $ \conf -> conf { maxIOB = iobAsNumber }

autoSenseEnableOrNot :: ManagerEnv
autoSenseEnableOrNot = do
    lang <- ask
    Tag onOrOff <- lift $ chooseOneItem lang
                                        (BoxSize (Height 15) (Width 60))
                                        (Locale.autoSenseTitle lang)
                                        (Locale.autoSenseMessage lang)
                                        (okButton lang)
                                        (cancelButton lang)
                                        (enabledOrDisabled lang)
    let decision = readT onOrOff :: Bool
    modify $ \conf -> conf { autoSenseEnabled = decision }

autoTuneEnableOrNot :: ManagerEnv
autoTuneEnableOrNot = do
    lang <- ask
    Tag onOrOff <- lift $ chooseOneItem lang
                                        (BoxSize (Height 15) (Width 60))
                                        (Locale.autoTuneTitle lang)
                                        (Locale.autoTuneMessage lang)
                                        (okButton lang)
                                        (cancelButton lang)
                                        (enabledOrDisabled lang)
    let decision = readT onOrOff :: Bool
    modify $ \conf -> conf { autoTuneEnabled = decision }

--showInstallationProgress :: ManagerEnv
--showInstallationProgress = do
    -- TODO: finish it.
--    lang <- ask
--    lift $ showProgress (BoxSize (Height 15) (Width 70))
--                        (notificationAboutCGMTitle lang)
--                        (notificationAboutCGMMessage lang)

-- | Prepares installation directory with all required subdirectories.
prepareInstallDirectory :: ManagerEnv
prepareInstallDirectory = do
    lift $         mkdir =<< pathToInstallDirectory
    lift $ mapM_ mkdir_p =<< pathsToSubDirectories
    tell "Install directory (with subdirs) is created."

-- | This is the last step of installation, we have to write configuration file.
writeConfigurationFile :: ManagerEnv
writeConfigurationFile = do
    createdConfiguration <- get
    storeConfiguration createdConfiguration

--installP :: ManagerEnv
--installP = do
--    lift $ sudo "ls" ["-l"]

-------------------------------------------------------------------------------
-- Helpers.
-------------------------------------------------------------------------------

enabledOrDisabled :: Localization -> [Item]
enabledOrDisabled lang =
    [ mkItem True  $ Locale.enabledLabel lang
    , mkItem False $ Locale.disabledLabel lang
    ]
