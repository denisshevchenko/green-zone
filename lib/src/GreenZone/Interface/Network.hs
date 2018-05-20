{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module GreenZone.Interface.Network
    ( turnWiFiOn
    , turnWiFiOff
    ) where

import           Control.Monad.IO.Class        ( MonadIO )
import           Data.Text                     ( Text )

import           GreenZone.Configuration.Types ( Configuration (..) )
import           GreenZone.Tool.Shell          ( SudoPassword (..),
                                                 runShCommandSudo )

turnWiFiOn :: MonadIO m => Configuration -> m ()
turnWiFiOn config = runShCommandSudo (SudoPassword pass) "ifup" [iface]
  where
    pass = sudoPassword config

turnWiFiOff :: MonadIO m => Configuration -> m ()
turnWiFiOff config = runShCommandSudo (SudoPassword pass) "ifdown" [iface]
  where
    pass = sudoPassword config

-- | It is assumed that SBC has only one WiFi-interface.
iface :: Text
iface = "wlan0" -- No! We have to take it from the system info!!
