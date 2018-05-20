{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GreenZone.Tool.IgnoreExceptions where

import           Control.Exception.Safe ( MonadCatch, SomeException, try )

-- | Hypothetically it's possible that some unexpected errors will occure during
-- some action. But sometimes it's not the reason for stopping GreenZone, so just
-- ignore them.
ignoreExceptions :: MonadCatch m => m () -> m ()
ignoreExceptions action = try action >>= \case
    Left (_ :: SomeException) -> return ()
    Right _                   -> return ()
