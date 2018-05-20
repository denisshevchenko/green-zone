{-# LANGUAGE Safe #-}

module GreenZone.Manager.Common.Types where

import           Data.Word       ( Word8 )

import           Data.Map.Strict ( Map )
import           Data.Text       ( Text )

-------------------------------------------------------------------------------
-- Dialogue boxes
-------------------------------------------------------------------------------

-- | Box size. It is a terminal pseudo-graphic, so the size is defined
-- in "cursor rectangles".
newtype Height  = Height  Word8
newtype Width   = Width   Word8
data    BoxSize = BoxSize Height Width

-- | Box title and primary message.
newtype Title   = Title Text
newtype Message = Message Text

-- | Items, we use them in menus.
newtype Tag   = Tag   Text
newtype Label = Label Text
data    Item  = Item  Tag Label

-- | Buttons labels.
newtype OkButton     = OkButton     Text
newtype CancelButton = CancelButton Text
newtype YesButton    = YesButton    Text
newtype NoButton     = NoButton     Text
newtype FixButton    = FixButton    Text

-------------------------------------------------------------------------------
-- Localization
-------------------------------------------------------------------------------

-- | Represents supported languages.
data Localization = Ru | En
    deriving (Read, Show, Eq, Ord)

-- | Contains all language-agnostic sentences we use during
-- installation.
data Sentence
    = OkButtonLabel
    | CancelButtonLabel
    | YesButtonLabel
    | NoButtonLabel
    | FixButtonLabel
    | InvalidDataTitle
    | CancelInstallationTitle
    | CancelInstallationMessage
    | LetSGetStartedTitle
    | LetSGetStartedMessage
    | IUnderstand
    | IDontWantToContinue
    | NotificationAboutPumpTitle
    | NotificationAboutPumpMessage
    | NotificationAboutCGMTitle
    | NotificationAboutCGMMessage
    | NotificationAboutSBCTitle
    | NotificationAboutSBCMessage
    | PumpSerialNumberTitle
    | PumpSerialNumberMessage
    | NightScoutHostTitle
    | NightScoutHostMessage
    | NightScoutAPISecretTitle
    | NightScoutAPISecretMessage
    | InvalidPumpSerialNumberMessage
    | InvalidNightScoutHostMessage
    | InvalidNightScoutAPISecretMessage
    | MaxIOBTitle
    | MaxIOBMessage
    | InvalidMaxIOBMessage
    | EnabledLabel
    | DisabledLabel
    | AutoSenseTitle
    | AutoSenseMessage
    | AutoTuneTitle
    | AutoTuneMessage
    | GreenZoneIsHereTitle
    | GreenZoneIsHereMessage
    | WelcomeTitle
    | WelcomeMessage
    | ReConfigureLabel
    | InstallLabel
    | DeleteLabel
    deriving (Eq, Ord)

-- | Localized versions of sentences.
type LocaleSentences = Map Sentence Text

-------------------------------------------------------------------------------
-- GreenZone Manager action.
-------------------------------------------------------------------------------

-- | What should we do with GreenZone?
data ManagerAction
    = InstallGreenZone
    | DeleteGreenZone
    | ReConfigureGreenZone
    deriving (Read, Show)
