{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

module GreenZone.Manager.Common.Boxes
    ( chooseOneItem
    , getUserInput
    , getUserPassword
    , showMessage
    , showProgress
    ) where

import           Control.Concurrent                    ( threadDelay )
import           Control.Monad                         ( void )
import           Data.Monoid                           ( (<>) )
import           Data.Version                          ( showVersion )
import           Data.Word                             ( Word8 )
import           GHC.IO.Handle                         ( BufferMode (..),
                                                         Handle, hClose,
                                                         hSetBuffering )
import           Prelude                               hiding ( FilePath )
import           System.IO                             ( hPrint )

import           Data.Text                             ( Text, empty, pack )
import           Shelly                                ( FilePath, Sh,
                                                         StdHandle (..),
                                                         StdStream (..), errExit,
                                                         errorExit, lastExitCode,
                                                         lastStderr, liftIO,
                                                         runHandles, run_,
                                                         sleep )

import           GreenZone.Manager.Common.Localization
import           GreenZone.Manager.Common.Types        ( BoxSize (..),
                                                         CancelButton (..),
                                                         FixButton (..),
                                                         Height (..), Item (..),
                                                         Label (..),
                                                         Localization (..),
                                                         Message (..),
                                                         NoButton (..),
                                                         OkButton (..), Tag (..),
                                                         Title (..), Width (..),
                                                         YesButton (..) )
import           GreenZone.Tool.Text                   ( showT )

import           Paths_green_zone                      ( version )

-- | User must choose exactly one item from the list of items.
chooseOneItem
    :: Localization
    -> BoxSize
    -> Title
    -> Message
    -> OkButton
    -> CancelButton
    -> [Item]
    -> Sh Tag
chooseOneItem lang size title (Message message) ok cancel items = do
    userChoice <- runDialogBox lang args noValidation (Message empty)
    return $ Tag userChoice
  where
    args = titlesArgs title
           ++ okAndCancelArgs ok cancel
           ++ ["--notags", "--menu", message]
           ++ boxSizeArgs size
           ++ numberOfItems
           ++ preparedItems
    preparedItems = concatMap prepareItem items
    prepareItem (Item (Tag aTag) (Label label)) = [aTag, label]
    numberOfItems = [showT . length $ items]

-- | Displays box with message and Ok-button.
showMessage
    :: Localization
    -> BoxSize
    -> Title
    -> Message
    -> YesButton
    -> NoButton
    -> Sh ()
showMessage lang size title (Message message) yes no =
    void $ runDialogBox lang args noValidation (Message empty)
  where
    args = titlesArgs title
           ++ yesAndNoArgs yes no
           ++ ["--yesno", message]
           ++ boxSizeArgs size

-- | Displays box with message and Ok-button.
showProgress
    :: BoxSize
    -> Title
    -> Message
    -> Sh ()
showProgress size title (Message message) =
    runHandles dialog args [ InHandle    CreatePipe
                           , OutHandle   Inherit
                           , ErrorHandle Inherit
                           ] $ \stdIn _ _ -> do
        liftIO $ hSetBuffering stdIn NoBuffering
        sleep 1
        increaseProgress 1 stdIn
  where
    args = titlesArgs title
           ++ ["--gauge", message]
           ++ boxSizeArgs size
           ++ ["0"]
    increaseProgress :: Word8 -> Handle -> Sh ()
    increaseProgress 100 hStdIn = liftIO $ do
        hPrint hStdIn (100 :: Word8)
        threadDelay 500000
        hClose hStdIn
    increaseProgress step hStdIn = do
        liftIO $ hPrint hStdIn step
        sleep 1
        increaseProgress (step + 1) hStdIn

-- | Gets an arbitrary line from the user.
getUserInput
    :: Localization
    -> BoxSize
    -> Title
    -> Message
    -> OkButton
    -> CancelButton
    -> Validator
    -> Message
    -> Sh Text
getUserInput lang size title (Message message) ok cancel =
    runDialogBox lang args
  where
    args = titlesArgs title
           ++ okAndCancelArgs ok cancel
           ++ ["--inputbox", message]
           ++ boxSizeArgs size

-- | Gets a password line from the user.
getUserPassword
    :: Localization
    -> BoxSize
    -> Title
    -> Message
    -> OkButton
    -> CancelButton
    -> Validator
    -> Message
    -> Sh Text
getUserPassword lang size title (Message message) ok cancel =
    runDialogBox lang args
  where
    args = titlesArgs title
           ++ okAndCancelArgs ok cancel
           ++ ["--passwordbox", message]
           ++ boxSizeArgs size

-------------------------------------------------------------------------------
-- Helpers.
-------------------------------------------------------------------------------

-- | Runs particular dialog box.
-- Continues if and only if the user explicitly moved forward
-- and inputed data is valid.
-- * If Cancel was clicked - we ask confirmation about cancelation.
-- * If inputed data is invalid - we show error notification.
runDialogBox
    :: Localization
    -> [Text]
    -> Validator
    -> Message
    -> Sh Text
runDialogBox lang args dataIsValid invalidDataMessage = do
    -- Don't throw an exception on non-zero exit code, just give me it.
    errExit False $ run_ dialog args
    lastExitCode >>= \case
        0 -> do
            dataFromUser <- lastStderr
            if dataIsValid dataFromUser
                then return dataFromUser
                else do
                    -- User's data is invalid, so we should provide another attempt...
                    errExit False $ run_ dialog invalidDataBox
                    lastExitCode >>= \case
                        1 -> goodbye
                        _ -> -- User is ready to fix its data, run the same dialog box again.
                             runDialogBox lang
                                          args
                                          dataIsValid
                                          invalidDataMessage
        _ -> do
            -- User clicked to Cancel, we have to make sure about cancelation.
            errExit False $ run_ dialog confirmBox
            lastExitCode >>= \case
                0 -> goodbye
                _ -> -- User changed mind, run the same dialog box again.
                     runDialogBox lang
                                  args
                                  dataIsValid
                                  invalidDataMessage
  where
    goodbye = errorExit "Installation was canceled by you, goodbye!"

    confirmBox = titlesArgs (cancelInstallationTitle lang)
                 ++ yesAndNoArgs (yesButton lang) (noButton lang)
                 ++ ["--defaultno", "--yesno", cancelMessageT]
                 ++ boxSizeArgs (BoxSize (Height 10) (Width 60))

    invalidDataBox = titlesArgs (invalidDataTitle lang)
                     ++ fixAndCancelArgs (fixButton lang) (cancelButton lang)
                     ++ ["--yesno", invalidDataMessageT]
                     ++ boxSizeArgs (BoxSize (Height 15) (Width 70))

    Message cancelMessageT = cancelInstallationMessage lang
    Message invalidDataMessageT = invalidDataMessage

-- | Name of Linux command for terminal dialogue.
dialog :: FilePath
dialog = "whiptail"

-- | Box title and backtitle (shown in the top left corner of screen).
-- This title is visible on all dialog boxes.
titlesArgs :: Title -> [Text]
titlesArgs (Title title) =
    [ "--backtitle", backTitle
    , "--title", title
    ]
  where
    backTitle = "GreenZone Manager " <> currentVersion
    currentVersion = pack . showVersion $ version

-- | Box size arguments.
boxSizeArgs :: BoxSize -> [Text]
boxSizeArgs (BoxSize (Height height) (Width width)) =
    [showT height, showT width]

-- | OK and CANCEL buttons arguments.
okAndCancelArgs :: OkButton -> CancelButton -> [Text]
okAndCancelArgs (OkButton ok) (CancelButton cancel) =
    ["--ok-button", ok, "--cancel-button", cancel]

-- | YES and NO buttons arguments.
yesAndNoArgs :: YesButton -> NoButton -> [Text]
yesAndNoArgs (YesButton yes) (NoButton no) =
    ["--yes-button", yes, "--no-button", no]

-- | FIX and CANCEL buttons arguments.
fixAndCancelArgs :: FixButton -> CancelButton -> [Text]
fixAndCancelArgs (FixButton fix) (CancelButton cancel) =
    ["--yes-button", fix, "--no-button", cancel]

-- | Function for validation of data from user.
-- Returns 'True' if data is valid.
type Validator = Text -> Bool

-- | If user's input shouldn't be validated, we use this fake.
noValidation :: Validator
noValidation _ = True
