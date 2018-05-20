{-# LANGUAGE Safe #-}

module GreenZone.Tool.Text where

import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Text              ( Text, pack, unpack )
import qualified Data.Text.IO           as TIO

-- | Enything that can be shown will be converted to 'Text'.
showT :: Show a => a -> Text
showT = pack . show

-- | Read a value from 'Text'.
-- Since 'read' is unsafe function, please make sure you
-- actually can read particular value from the text.
readT :: Read a => Text -> a
readT = read . unpack

-- | Print text in terminal.
putText :: MonadIO m => Text -> m ()
putText = liftIO . TIO.putStrLn
