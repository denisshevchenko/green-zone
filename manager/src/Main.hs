{-|
Module      : Main
Description : Main module of GreenZone Manager.

This is the main module of GreenZone Manager.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe       #-}

module Main
    ( main
    ) where

import           GreenZone.Manager.Common.CLI           ( checkCLIOptions )
import           GreenZone.Manager.Common.Types         ( ManagerAction (..) )
import           GreenZone.Manager.Delete.Dialogue      ( startDeleteDialogue )
import           GreenZone.Manager.Init.Dialogue        ( chooseALanguage,
                                                          chooseAnAction )
import           GreenZone.Manager.Install.Dialogue     ( startInstallDialogue )
import           GreenZone.Manager.ReConfigure.Dialogue ( startReConfigureDialogue )

main :: IO ()
main = do
    checkCLIOptions
    chosenLang <- chooseALanguage
    chooseAnAction chosenLang >>= \case
        InstallGreenZone     -> startInstallDialogue     chosenLang
        ReConfigureGreenZone -> startReConfigureDialogue chosenLang
        DeleteGreenZone      -> startDeleteDialogue      chosenLang
