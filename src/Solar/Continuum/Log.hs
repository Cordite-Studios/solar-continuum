{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Solar.Continuum.Log where

import Control.Monad.Free
import Solar.Continuum.LogF
import Solar.Continuum.Types
import Data.Int
import qualified Data.ByteString as B

-- | Use this in your declarations for Continuum Logs
type Log = Free LogF
-- | Used for functions that operate within the Free Log monad.
type LogM = MonadFree LogF

-- | Tests for whether or not the log exists (logically) at this point
logExists   :: (LogM m)
            => LogName -- ^ Identifier of the log
            -> m Bool -- ^ Result of whether or not it exists
logExists n = liftF $ LogExists n id

-- | Tells the current read position.
-- You may never read past what has been confirmed written.
-- In other words, you may not read what you write until you commit it.
tellLogReadPosition :: (LogM m)
                    => LogName -- ^ Identifier of the log
                    -> m Int64
tellLogReadPosition n = liftF $ TellLogReadPosition n id

-- | Tells the current write position.
-- Is practically never known until it is resolved by an finalizing resolver.
-- Such resolvers hopefully can handle cyclical references.
tellLogWritePosition    :: (LogM m)
                        => LogName  -- ^ Identifier of the log
                        -> m LogPos
tellLogWritePosition n = liftF $ TellLogWritePosition n id

-- | Gets the checksum of the (already written) log.
-- Will never take into account writes that happen within the current execution.
-- However all deletes must later result in empty checksums if in the same execution
checkSumLog :: (LogM m)
            => LogName  -- ^ Identifier of the log
            -> m B.ByteString -- ^ A checksum, which can be written to in a later append.
checkSumLog n = liftF $ CheckSumLog n id

-- | Moves the reading position back to the start of the log.
resetLogPosition    :: (LogM m)
                    => LogName  -- ^ Identifier of the log
                    -> m ()
resetLogPosition n = liftF $ ResetLogPosition n ()

-- | Skips forward a specified number of bytes.
-- Not recommended unless the position comes from another log.
skipForwardLog  :: (LogM m)
                => LogName -- ^ Identifier of the log
                -> Int64
                -> m ()
skipForwardLog n d = liftF $ SkipForwardLog n d ()

-- | Deletes the log specified.
-- Any writes to a log that is later deleted within the same execution can probably
-- be ignored.
deleteLog   :: (LogM m)
            => LogName  -- ^ Identifier of the log
            -> m ()
deleteLog n = liftF $ DeleteLog n ()

-- | Renames the log.
-- Implementor beware when it comes to deletes and renames 
renameLog   :: (LogM m)
            => LogName
            -> LogName
            -> m ()
renameLog n1 n2 = liftF $ RenameLog n1 n2 ()

-- | Rea
readFromLog :: (LogM m, Loggable a)
            => LogName
            -> m (TryRead a)
readFromLog n = liftF $ ReadFromLog n id

-- | Appends data to the log.
-- If it has references (positions) to other logs, the implementor should
-- resolve them using the Loggable typeclass's 'resolvePositions'.
appendLogData   :: (LogM m, Loggable a)
                => LogName -- ^ Identifier of the log
                -> a -- ^ Serializeable and resolveable data type
                -> m ()
appendLogData n d = liftF $ AppendLogData n d ()
