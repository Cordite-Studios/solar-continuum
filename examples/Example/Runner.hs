{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Example.Runner where

import Control.Monad.Free

import Solar.Continuum.Log
import Solar.Continuum.LogF
import Solar.Continuum.Types


runLogIO :: Log a -> IO a
runLogIO = iterM run where
    run :: LogF (IO a) -> IO a
    run (LogExists _ next) = next False
    run (TellLogReadPosition _ next) = next 0
    run (TellLogWritePosition _ next) = next $ Known 0
    run (TellLogSize _ next) = next 0
    run (CheckSumLog _ next) = next ""
    run (ResetLogPosition _ next) = next
    run (SkipForwardLog _ _ next) = next
    run (DeleteLog _ next) = next
    run (RenameLog _ _ next) = next LogNotFound
    run (ReadFromLog _ next) = next $ Left "Worst runner ever."
    run (AppendLogData _ _ next) = next
