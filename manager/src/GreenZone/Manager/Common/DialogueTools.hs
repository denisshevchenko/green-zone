{-# LANGUAGE Safe #-}

module GreenZone.Manager.Common.DialogueTools where

import           GreenZone.Manager.Common.Types ( Item (..), Label, Tag (..) )
import           GreenZone.Tool.Text            ( showT )

-- | Helper for more convenient creation of menu items.
mkItem :: Show t => t -> Label -> Item
mkItem tagValue = Item (Tag $ showT tagValue)
