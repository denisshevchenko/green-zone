{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module GreenZone.Manager.Common.Localization.En where

import           Data.Monoid                    ( (<>) )

import qualified Data.Map.Strict                as Map

import           GreenZone.Manager.Common.Types ( LocaleSentences,
                                                  Sentence (..) )

sentencesEn :: LocaleSentences
sentencesEn = Map.fromList
    [ (OkButtonLabel,                       "Ok")
    , (CancelButtonLabel,                   "Cancel")
    , (YesButtonLabel,                      "Yes")
    , (NoButtonLabel,                       "No")
    , (FixButtonLabel,                      "Fix")
    , (InvalidDataTitle,                    "Invalid Data")
    , (CancelInstallationTitle,             "Cancel Installation?")
    , (CancelInstallationMessage,           "Are you sure you want to cancel installation?")
    , (LetSGetStartedTitle,                 "Let's get started!")
    , (LetSGetStartedMessage,               "During installation you will be asked about\n"
                                            <> "your hardware, NightScout settings, etc.\n"
                                            <> "You should provide your sudo-password as well,\n"
                                            <> "because installation of third-party software is\n"
                                            <> "required for GreenZone.")
    , (IUnderstand,                         "Continue")
    , (IDontWantToContinue,                 "Cancel")
    , (NotificationAboutPumpTitle,          "Notification about Pump")
    , (NotificationAboutPumpMessage,        "Currently the only pump supported by GreenZone is\n"
                                            <> "Medtronic MMT Paradigm Real Time 722.\n"
                                            <> "List of supported pumps will be expanded in the future.")
    , (NotificationAboutCGMTitle,           "Notification about CGM")
    , (NotificationAboutCGMMessage,         "Currently the only CGM supported by GreenZone is\n"
                                            <> "Medtronic MiniLink MMT-7703.\n"
                                            <> "List of supported CGMs will be expanded in the future.")
    , (NotificationAboutSBCTitle,           "Notification about SBC")
    , (NotificationAboutSBCMessage,         "It is assumed that GreenZone is running on Intel Edison with\n"
                                            <> "900 MHz Explorer Board.\n"
                                            <> "GreenZone installation on other SBCs will be supported in the future.")
    , (PumpSerialNumberTitle,               "Pump Serial Number")
    , (PumpSerialNumberMessage,             "Please input serial number of your pump (6 digits)")
    , (NightScoutHostTitle,                 "NightScout Website")
    , (NightScoutHostMessage,               "Please input your NightScout website (i.e. https://yourname.herokuapp.com/)")
    , (NightScoutAPISecretTitle,            "NightScout API Secret")
    , (NightScoutAPISecretMessage,          "Please input your NightScout API Secret (you specified it during\n"
                                            <> "deployment of your NightScout website)")
    , (InvalidPumpSerialNumberMessage,      "Pump serial number must contain 6 digits.")
    , (InvalidNightScoutHostMessage,        "It doesn't look like NightScout website address.")
    , (InvalidNightScoutAPISecretMessage,   "NightScout API Secret must contain at least 12 characters.")
    , (MaxIOBTitle,                         "Maximum IOB")
    , (MaxIOBMessage,                       "Please input maximum IOB (Insulin-on-Board), positive integer.\n")
    , (InvalidMaxIOBMessage,                "Maximum IOB should be a positive integer.")
    , (EnabledLabel,                        "Enable")
    , (DisabledLabel,                       "Disable")
    , (AutoSenseTitle,                      "AutoSense")
    , (AutoSenseMessage,                    "Enable automatic sensitivity adjustment?")
    , (AutoTuneTitle,                       "AutoTune")
    , (AutoTuneMessage,                     "Enable autotuning of basals and ratios?")
    , (GreenZoneIsHereTitle,                "Another GreenZone?")
    , (GreenZoneIsHereMessage,              "It seems that GreenZone is already installed on this SBC.\n"
                                            <> "ATTENTION: If you continue installation, previous version of \n"
                                            <> "GreenZone will be deleted.")
    , (WelcomeTitle,                        "Welcome!")
    , (WelcomeMessage,                      "This program will install GreenZone on your SBC.\n"
                                            <> "Please choose the language to be used for the installation.")
    , (ReConfigureLabel,                    "Re-configure GreenZone")
    , (InstallLabel,                        "Install GreenZone")
    , (DeleteLabel,                         "Delete GreenZone")
    ]
