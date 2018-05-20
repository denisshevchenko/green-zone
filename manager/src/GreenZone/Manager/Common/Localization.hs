{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module GreenZone.Manager.Common.Localization where

import           Data.Map.Strict                          ( Map, (!) )
import qualified Data.Map.Strict                          as M
import           Data.Text                                ( Text )

import           GreenZone.Manager.Common.Localization.En ( sentencesEn )
import           GreenZone.Manager.Common.Localization.Ru ( sentencesRu )
import           GreenZone.Manager.Common.Types           ( CancelButton (..),
                                                            FixButton (..),
                                                            Label (..),
                                                            LocaleSentences,
                                                            Localization (..),
                                                            Message (..),
                                                            NoButton (..),
                                                            OkButton (..),
                                                            Sentence (..),
                                                            Title (..),
                                                            YesButton (..) )

sentences :: Map Localization LocaleSentences
sentences = M.fromList
    [ (En, sentencesEn)
    , (Ru, sentencesRu)
    ]

langCodes :: [(Text, (Localization, Label))]
langCodes =
    [ ("en", (En, Label "English"))
    , ("ru", (Ru, Label "Русский"))
    ]

defaultLang :: Localization
defaultLang = En

iUnderstand :: Localization -> Text
iUnderstand lang = sentences ! lang ! IUnderstand

iDontWantToContinue :: Localization -> Text
iDontWantToContinue lang = sentences ! lang ! IDontWantToContinue

okButton :: Localization -> OkButton
okButton lang = OkButton $ sentences ! lang ! OkButtonLabel

cancelButton :: Localization -> CancelButton
cancelButton lang = CancelButton $ sentences ! lang ! CancelButtonLabel

yesButton :: Localization -> YesButton
yesButton lang = YesButton $ sentences ! lang ! YesButtonLabel

noButton :: Localization -> NoButton
noButton lang = NoButton $ sentences ! lang ! NoButtonLabel

fixButton :: Localization -> FixButton
fixButton lang = FixButton $ sentences ! lang ! FixButtonLabel

enabledLabel
  , disabledLabel
  , reConfigureLabel
  , installLabel
  , deleteLabel
    :: Localization -> Label
enabledLabel     lang = Label $ sentences ! lang ! EnabledLabel
disabledLabel    lang = Label $ sentences ! lang ! DisabledLabel
reConfigureLabel lang = Label $ sentences ! lang ! ReConfigureLabel
installLabel     lang = Label $ sentences ! lang ! InstallLabel
deleteLabel      lang = Label $ sentences ! lang ! DeleteLabel

invalidDataTitle
  , cancelInstallationTitle
  , letSGetStartedTitle
  , notificationAboutPumpTitle
  , notificationAboutCGMTitle
  , notificationAboutSBCTitle
  , pumpSerialNumberTitle
  , nightScoutHostTitle
  , nightScoutAPISecretTitle
  , maxIOBTitle
  , autoSenseTitle
  , autoTuneTitle
  , greenZoneIsHereTitle
  , welcomeTitle
    :: Localization -> Title
invalidDataTitle           lang = Title $ sentences ! lang ! InvalidDataTitle
cancelInstallationTitle    lang = Title $ sentences ! lang ! CancelInstallationTitle
letSGetStartedTitle        lang = Title $ sentences ! lang ! LetSGetStartedTitle
notificationAboutPumpTitle lang = Title $ sentences ! lang ! NotificationAboutPumpTitle
notificationAboutCGMTitle  lang = Title $ sentences ! lang ! NotificationAboutCGMTitle
notificationAboutSBCTitle  lang = Title $ sentences ! lang ! NotificationAboutSBCTitle
pumpSerialNumberTitle      lang = Title $ sentences ! lang ! PumpSerialNumberTitle
nightScoutHostTitle        lang = Title $ sentences ! lang ! NightScoutHostTitle
nightScoutAPISecretTitle   lang = Title $ sentences ! lang ! NightScoutAPISecretTitle
maxIOBTitle                lang = Title $ sentences ! lang ! MaxIOBTitle
autoSenseTitle             lang = Title $ sentences ! lang ! AutoSenseTitle
autoTuneTitle              lang = Title $ sentences ! lang ! AutoTuneTitle
greenZoneIsHereTitle       lang = Title $ sentences ! lang ! GreenZoneIsHereTitle
welcomeTitle               lang = Title $ sentences ! lang ! WelcomeTitle

cancelInstallationMessage
  , letSGetStartedMessage
  , notificationAboutPumpMessage
  , notificationAboutCGMMessage
  , notificationAboutSBCMessage
  , pumpSerialNumberMessage
  , nightScoutHostMessage
  , nightScoutAPISecretMessage
  , invalidPumpSerialNumberMessage
  , invalidNightScoutHostMessage
  , invalidNightScoutAPISecretMessage
  , maxIOBMessage
  , invalidMaxIOBMessage
  , autoSenseMessage
  , autoTuneMessage
  , greenZoneIsHereMessage
  , welcomeMessage
    :: Localization -> Message
cancelInstallationMessage         lang = Message $ sentences ! lang ! CancelInstallationMessage
letSGetStartedMessage             lang = Message $ sentences ! lang ! LetSGetStartedMessage
notificationAboutPumpMessage      lang = Message $ sentences ! lang ! NotificationAboutPumpMessage
notificationAboutCGMMessage       lang = Message $ sentences ! lang ! NotificationAboutCGMMessage
notificationAboutSBCMessage       lang = Message $ sentences ! lang ! NotificationAboutSBCMessage
pumpSerialNumberMessage           lang = Message $ sentences ! lang ! PumpSerialNumberMessage
nightScoutHostMessage             lang = Message $ sentences ! lang ! NightScoutHostMessage
nightScoutAPISecretMessage        lang = Message $ sentences ! lang ! NightScoutAPISecretMessage
invalidPumpSerialNumberMessage    lang = Message $ sentences ! lang ! InvalidPumpSerialNumberMessage
invalidNightScoutHostMessage      lang = Message $ sentences ! lang ! InvalidNightScoutHostMessage
invalidNightScoutAPISecretMessage lang = Message $ sentences ! lang ! InvalidNightScoutAPISecretMessage
maxIOBMessage                     lang = Message $ sentences ! lang ! MaxIOBMessage
invalidMaxIOBMessage              lang = Message $ sentences ! lang ! InvalidMaxIOBMessage
autoSenseMessage                  lang = Message $ sentences ! lang ! AutoSenseMessage
autoTuneMessage                   lang = Message $ sentences ! lang ! AutoTuneMessage
greenZoneIsHereMessage            lang = Message $ sentences ! lang ! GreenZoneIsHereMessage
welcomeMessage                    lang = Message $ sentences ! lang ! WelcomeMessage
