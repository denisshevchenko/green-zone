{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe          #-}

module GreenZone.Loop.Types where

import           GHC.Generics ( Generic )

import           Data.Text    ( Text )

-- |
data GlucoseEntry = GlucoseEntry
    { pathToConfig :: Int
    } deriving (Show, Generic)

newtype GlucoseEntryError = GlucoseEntryError Text
    deriving Show
