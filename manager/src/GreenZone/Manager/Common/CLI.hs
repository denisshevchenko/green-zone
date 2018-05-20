{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe       #-}

module GreenZone.Manager.Common.CLI
    ( checkCLIOptions
    ) where

import           Data.Monoid            ( (<>) )
import           Data.Version           ( showVersion )
import           System.Environment     ( getArgs, getProgName )

import           Control.Monad.IO.Class ( MonadIO, liftIO )

import           GreenZone.Exit         ( exitWithoutRestart )
import           Paths_green_zone       ( version )

-- | Gets path to configuration file (or, probably, shows help or version end exit).
checkCLIOptions :: MonadIO m => m ()
checkCLIOptions = liftIO getArgs >>= \case
    []             -> return ()
    ["-h"]         -> justPrintHelp
    ["--help"]     -> justPrintHelp
    ["-v"]         -> justPrintVersion
    ["--version"]  -> justPrintVersion
    _              -> justPrintHelp

justPrintHelp :: MonadIO m => m a
justPrintHelp = liftIO $ do
    name <- getProgName
    putStrLn $ helpInfo name
    exitWithoutRestart

justPrintVersion :: MonadIO m => m a
justPrintVersion = liftIO $ do
    putStrLn $ showVersion version
    exitWithoutRestart

helpInfo :: String -> String
helpInfo progName = "Usage:\n " <> progName
