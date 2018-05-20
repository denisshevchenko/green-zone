{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

module GreenZone.Tool.Shell where

import           Control.Monad.IO.Class ( MonadIO )
import           Data.Text              ( Text )
import           Shelly                 ( errExit, fromText, run, run_, shelly,
                                          (-|-) )

newtype SudoPassword = SudoPassword Text

runShCommand
    :: MonadIO m
    => Text
    -> [Text]
    -> m ()
runShCommand command arguments = shelly $
    errExit False $ run_ (fromText command) arguments

-- | @sudo@-variant for 'runShellCommand'. @sudo@ will ask
-- a password from @stdin@, because of option @-S@.
runShCommandSudo
    :: MonadIO m
    => SudoPassword
    -> Text
    -> [Text]
    -> m ()
runShCommandSudo (SudoPassword password) command arguments = shelly $
    run "echo" [password] -|-
    (errExit False $ run_ "sudo" (["-S", "--", command] ++ arguments))

runShCommandWithResult
    :: MonadIO m
    => Text
    -> [Text]
    -> m Text
runShCommandWithResult command arguments = shelly $
    errExit False $ run (fromText command) arguments
