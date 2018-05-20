module Main
    ( main
    ) where

import           Test.Hspec ( hspec )

import qualified CommonSpec as Common

main :: IO ()
main = hspec $ do
    Common.spec
