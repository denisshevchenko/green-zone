{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}
{-# LANGUAGE ViewPatterns      #-}

module GreenZone.Configuration.Validators
    ( ErrorReport (..)
    , validate
      -- Validators for GreenZone Manager dialogues.
    , pumpSerialNumberValidator
    , nightScoutHostValidator
    , nightScoutAPISecretValidator
    , maxIOBValidator
    ) where

import           Data.Char                     ( isControl, isDigit, isSpace )
import           Data.Monoid                   ( (<>) )
import           Data.Word                     ( Word8 )
import           Prelude                       hiding ( max, min )
import           Text.Read                     ( readMaybe )

import           Data.Text                     ( Text )
import qualified Data.Text                     as T

import           GreenZone.Configuration.Types ( Configuration (..),
                                                 DelayInMinutes (..),
                                                 InternalConstants (..),
                                                 Logging (..), LoopDelay (..),
                                                 NightScoutConfiguration (..),
                                                 Percentage (..),
                                                 PumpConfiguration (..),
                                                 SizeInBytes (..),
                                                 TemperatureInC (..) )
import           GreenZone.Tool.Text           ( showT )

{-|
Configuration is built from values that was read from the configuration file.
It's possible that the file was changed manually, so we cannot rely on its
correctness and must validate configuration values explicitly.

Since validation will be performed after Yaml-decoding, we already know that
common structure is valid (Aeson guarantees it, so numbers are definitely
numbers, and Word8 are definitely not a Double). So we must check values
themselves.

Some of these validators are using during GreenZone installation as well.
-}

-- | Contains a list of errors, can be empty.
newtype ErrorReport = ErrorReport [Text]

validate :: Configuration -> Maybe ErrorReport
validate config =
    if null errors
        then Nothing
        else Just $ ErrorReport errors
  where
    errors = filter (not . T.null) reports
    -- Reports: particular report is empty if everything is OK.
    reports =
        [ validateLoopDelay           (forSBC   . loopDelay      $ internalConfig)
        , validateLoopDelay           (forCGM   . loopDelay      $ internalConfig)
        , validateLoopDelay           (forCloud . loopDelay      $ internalConfig)
        , validateLoopDelay           (forPump  . loopDelay      $ internalConfig)
        , validatePumpSerialNumber    (serialNumber . pump       $ config)
        , validateMaxIOB              (maxIOB                      config)
        , validateNightScoutHost      (host      . nightScout    $ config)
        , validateNightScoutAPISecret (apiSecret . nightScout    $ config)
        , validateMaxLogSize          (maxLogSize      . logging $ internalConfig)
        , validateMaxNumberOfLogs     (maxNumberOfLogs . logging $ internalConfig)
        , validateLoopDelay           (archivistDelay  . logging $ internalConfig)
        , validateMinBatteryLevel     (minBatteryLevel             internalConfig)
        , validateMaxCPUTemperature   (maxCPUTemperature           internalConfig)
        ]
    internalConfig = internal config

pumpSerialNumberValidator :: Text -> Bool
pumpSerialNumberValidator serialNum = validatePumpSerialNumber serialNum == mempty

nightScoutHostValidator :: Text -> Bool
nightScoutHostValidator aHost = validateNightScoutHost aHost == mempty

nightScoutAPISecretValidator :: Text -> Bool
nightScoutAPISecretValidator secret = validateNightScoutAPISecret secret == mempty

maxIOBValidator :: Text -> Bool
maxIOBValidator (T.unpack . T.strip -> realIOB) =
    case readMaybe realIOB :: Maybe Word8 of
        Nothing  -> False
        Just iob -> validateMaxIOB iob == mempty

-------------------------------------------------------------------------------
-- Internal validators.
-------------------------------------------------------------------------------

validatePumpSerialNumber :: Text -> Text
validatePumpSerialNumber (T.strip -> text) =
    if T.length text == size && T.all isDigit text
        then mempty
        else "pump serial number should consist of " <> showT size <> " digits"
  where
    size = 6

validateNightScoutHost :: Text -> Text
validateNightScoutHost (T.strip -> text) =
    if itLooksLikeAnURL
        then mempty
        else "NightScout host should be a complete URL"
  where
    -- We don't need a complete URL validation here (according to RFC1738),
    -- because we know that user will work with NightScout (or similar application).
    -- So just check prefix and (some) unacceptable characters.
    itLooksLikeAnURL = correctPrefix && noUnacceptableChars
    correctPrefix =
        "http://"  `T.isPrefixOf` text ||
        "https://" `T.isPrefixOf` text
    noUnacceptableChars =
        T.all (not . isControl) text &&
        T.all (not . isSpace)   text

validateNightScoutAPISecret :: Text -> Text
validateNightScoutAPISecret (T.strip -> text) =
    if T.length text >= min
        then mempty
        else "length of NightScout API secret should be " <> showT min <> " characters or more"
  where
    min = 12

validateLoopDelay :: DelayInMinutes -> Text
validateLoopDelay (DelayInMinutes delay) =
    if delay > min && delay < max
        then mempty
        else "loop delay should be from " <> showT min <> " to " <> showT max
  where
    min = 0
    max = 90

validateMinBatteryLevel :: Percentage -> Text
validateMinBatteryLevel (Percentage p) =
    if p > min && p < max
        then mempty
        else "minimal battery level should be from " <> showT min <> " to " <> showT max
  where
    min = 0
    max = 50

validateMaxCPUTemperature :: TemperatureInC -> Text
validateMaxCPUTemperature (TemperatureInC t) =
    if t > min && t < max
        then mempty
        else "maximal CPU temperature should be from " <> showT min <> " to " <> showT max
  where
    min = 30.0
    max = 100.0

validateMaxLogSize :: SizeInBytes -> Text
validateMaxLogSize (SizeInBytes size) =
    if size > min && size < max
        then mempty
        else "maximal size of one log should be from " <> showT min <> " to " <> showT max
  where
    min = oneMB
    max = oneMB * 10
    oneMB   = 1024 * 1024

validateMaxNumberOfLogs :: Word8 -> Text
validateMaxNumberOfLogs num =
    if num > min && num < max
        then mempty
        else "maximal number of logs should be from " <> showT min <> " to " <> showT max
  where
    min = 1
    max = 30

validateMaxIOB :: Word8 -> Text
validateMaxIOB iob =
    if iob > min && iob < max
        then mempty
        else "maximal IOB should be from " <> showT min <> " to " <> showT max
  where
    min = 0
    max = 20
