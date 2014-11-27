{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Solar.Continuum.LogF where

import qualified Data.ByteString as B

import Data.Int
import Solar.Continuum.Types

  
-- | The continuation passing data structure for the Log Free Monad
data LogF next
    -- Checks
    = LogExists LogName (Bool -> next)
    -- Info
    | TellLogReadPosition LogName (Int64 -> next)
    | TellLogWritePosition LogName (LogPos -> next)
    | TellLogSize LogName (Int64 -> next)
    | CheckSumLog LogName (B.ByteString -> next)
    -- Position altering
    | ResetLogPosition LogName next
    | SkipForwardLog LogName Int64 next
    | DeleteLog LogName next
    | RenameLog LogName LogName next
    | forall a. ReadFromLog LogName (TryRead a -> next)
    | forall a. Loggable a => AppendLogData LogName a next

-- | We can't do existentials with derive functor, so here's what's needed for Free.
instance Functor LogF where
    fmap f (LogExists n a) = LogExists n $ f . a
    fmap f (TellLogReadPosition n a) = TellLogReadPosition n $ f . a
    fmap f (TellLogWritePosition n a) = TellLogWritePosition n $ f . a
    fmap f (TellLogSize n a) = TellLogSize n $ f . a
    fmap f (CheckSumLog n a) = CheckSumLog n $ f . a
    fmap f (ResetLogPosition n a) = ResetLogPosition n $ f a
    fmap f (SkipForwardLog n p a) = SkipForwardLog n p $ f a
    fmap f (DeleteLog n a) = DeleteLog n $ f a
    fmap f (RenameLog n1 n2 a) = RenameLog n1 n2 $ f a
    fmap f (ReadFromLog n a) = ReadFromLog n $ f . a
    fmap f (AppendLogData n d a) = AppendLogData n d $ f a
