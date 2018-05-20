{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module GreenZone.Exit where

import           Control.Monad            ( void )
import           Data.Monoid              ( (<>) )

import           Control.Concurrent.Async ( Async, uninterruptibleCancel )
import           System.Exit              ( exitFailure, exitSuccess )
import           System.Posix.Signals     ( Handler (..), installHandler,
                                            sigINT, sigTERM )

import           Control.Monad.IO.Class   ( MonadIO, liftIO )

import           GreenZone.Cache.Itself   ( writeCacheToFile )
import           GreenZone.Cache.Types    ( SharedCache )
import           GreenZone.Log            ( LogLevel (..), toGlobalLog )

{-|
Correct exit for GreenZone program.

GreenZone is designed for "perpetual motion", so exit is treated as a problem.
There're two possible options why GreenZone can exit:

    1. CPU overheat or battery discharge,
    2. Some unexpected error.

The first option is treated as a critical problem which requires some actions
from the user (for example, SBC cooling or battery changing). In this case
GreenZone cannot continue its work automatically, because it's assumed that the
user, after problem fixing, will restart SBC manually.

The second one is treated as a common problem. In this case GreenZone will be
restarted automatically by the @systemd@. Please see 'Systemd' subdirectory for
more details.
-}
exitWithoutRestart, exitWithRestart :: MonadIO m => m a
exitWithoutRestart = liftIO exitSuccess
exitWithRestart    = liftIO exitFailure

{-|
When user launched GreenZone in non-daemon mode, he can stop it using
Ctrl+C shortcut. On the POSIX systems it sends system signal SIGINT to the
process. To guarantee correct exit of GreenZone, we must handle this signal.
There is a handler for SIGTERM as well.
-}
setupSignalsHandling
    :: MonadIO m
    => Async a
    -> SharedCache
    -> m ()
setupSignalsHandling loop cache = do
    setupHandlerFor sigINT
    setupHandlerFor sigTERM
  where
    setupHandlerFor signal = liftIO $
        void $ installHandler signal (CatchOnce (handlerFor signal)) Nothing

    handlerFor signal = do
        -- Cancel just one loop, all other ones will be canceled automatically.
        uninterruptibleCancel loop
        writeCacheToFile cache
        toGlobalLog WARNING $ "GreenZone caught " <> showSignal <> " signal."
        exitWithoutRestart
      where
        showSignal = if | signal == sigINT  -> "SIGINT"
                        | signal == sigTERM -> "SIGTERM"
