{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import           Control.Monad                ( mapM )
import           Data.Monoid                  ( (<>) )

import           Control.Concurrent.Async     ( async, waitAnyCatchCancel )
import           Control.Exception.Safe       ( SomeException, try )

import           GreenZone.Cache.Itself       ( readCacheFromFile )
import           GreenZone.CLI                ( getPathToConfig )
import           GreenZone.Configuration.File ( readConfiguration )
import           GreenZone.Exit               ( exitWithRestart,
                                                exitWithoutRestart,
                                                setupSignalsHandling )
import           GreenZone.Log                ( LogLevel (..), toGlobalLog,
                                                toGlobalLogInit )
import           GreenZone.Log.Archivist      ( logArchivistLoop )
import           GreenZone.Loop.CGM           ( cgmLoop )
import           GreenZone.Loop.Cloud         ( cloudLoop )
import           GreenZone.Loop.Pump          ( pumpLoop )
import           GreenZone.Loop.SBCSupervisor ( sbcSupervisorLoop )
import           GreenZone.Tool.Text          ( showT )

main :: IO ()
main = do
    toGlobalLogInit
    pathToConfig <- getPathToConfig
    try (readConfiguration pathToConfig) >>= \case
        Left (problem :: SomeException) -> do
            toGlobalLog ERROR $ showT problem
            exitWithoutRestart
        Right config -> do
            cache <- readCacheFromFile
            sbcLoop <- async $ sbcSupervisorLoop config
            setupSignalsHandling sbcLoop cache
            mainLoops <- mapM async [ pumpLoop         config cache
                                    , cgmLoop          config cache
                                    , cloudLoop        config cache
                                    , logArchivistLoop config
                                    ]
            let loops = sbcLoop : mainLoops
            toGlobalLog INFO "GreenZone is launched"

            (stoppedLoop, whatHappened) <- waitAnyCatchCancel loops
            if stoppedLoop == sbcLoop
                then do
                    toGlobalLog WARNING $ "GreenZone is stopped: " <> showT whatHappened
                    exitWithoutRestart
                else do
                    toGlobalLog ERROR $ "GreenZone is stopped: " <> showT whatHappened
                    exitWithRestart
