{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module Example.Runner where

import System.IO
import Data.IORef
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Serialize as S
import qualified Data.Map as M
import qualified Data.Set as ST

import Control.Monad.Free

import Data.Int

import Solar.Continuum.Log
import Solar.Continuum.LogF
import Solar.Continuum.Types

data Logging = forall a. Loggable a => MkLogging a

packLogger :: Loggable a => a -> Logging
packLogger = MkLogging

data State = State
    { toDelete :: IORef (ST.Set LogName)
    , toAppend :: IORef (M.Map LogName B.ByteString)
    , nextCalc :: IORef Int
    , positions :: IORef (M.Map LogName (ST.Set Int))
    , known :: IORef (M.Map Int Int64)
    , unknown :: IORef (M.Map Int Logging)
    }

mkState :: IO State
mkState = do
    uk <- newIORef M.empty
    k <- newIORef M.empty
    p <- newIORef M.empty
    n <- newIORef 0
    a <- newIORef M.empty
    d <- newIORef ST.empty
    return $ State d a n p k uk

runLogIO :: Log a -> IO a
runLogIO log = do
    state <- mkState
    res <- iterM (run state) log
    commit state
    return res
    where
    run :: State -> LogF (IO a) -> IO a
    run state (LogExists _ next) = next False
    run state (TellLogReadPosition _ next) = next 0
    run state (TellLogWritePosition _ next) = next $ Known 0
    run state (TellLogSize _ next) = next 0
    run state (CheckSumLog _ next) = next ""
    run state (ResetLogPosition _ next) = next
    run state (SkipForwardLog _ _ next) = next
    run state (DeleteLog _ next) = next
    run state (RenameLog _ _ next) = next LogNotFound
    run state (ReadFromLog _ next) = next $ Left "Worst runner ever."
    run state (AppendLogData name dat next) = do
        ident <- readIORef $ nextCalc state
        when (unresolvedPositions dat > 0) $ do
            modifyIORef' (unknown state) $ M.insert ident $ packLogger dat
            return ()
        -- TODO: plenty
        modifyIORef' (nextCalc state) succ
        next
    commit state = do
        return ()
