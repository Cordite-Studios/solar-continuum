{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module Example.Runner where

import System.IO
import Data.IORef
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as S
import qualified Data.Map as M
import qualified Data.Set as ST
import qualified Data.Foldable as DF
import System.Directory
import Crypto.Hash.MD5

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
    , renames :: IORef (M.Map LogName LogName)
    , readPosition :: IORef (M.Map LogName Int)
    , unknown :: IORef (M.Map Int (Int, Logging))
    }

mkState :: IO State
mkState = do
    d <- newIORef ST.empty -- toDelete
    a <- newIORef M.empty -- toAppend
    n <- newIORef 0 -- nextCalc
    p <- newIORef M.empty -- positions
    r <- newIORef M.empty -- renames
    rp <- newIORef M.empty -- readPosition
    uk <- newIORef M.empty -- unknown
    
    return $ State d a n p r rp uk

fName :: B.ByteString -> String
fName = show

runLogIO :: Log a -> IO a
runLogIO log = do
    state <- mkState
    res <- iterM (run state) log
    commit state
    return res
    where
    run :: State -> LogF (IO a) -> IO a
    run state (LogExists name next) = do
        exists <- doesFileExist $ fName name
        if exists
        then do
            deleted <- readIORef $ toDelete state
            next $ not $ ST.member name deleted
        else do
            append <- readIORef $ toAppend state
            case M.lookup name append of
                Nothing -> next False
                Just _ -> next True
    run state (TellLogReadPosition name next) = do
        position <- readIORef $ readPosition state
        case M.lookup name position of
            Nothing -> next 0
            Just x -> next $ fromIntegral x
    run state (TellLogWritePosition name next) = do
        u <- readIORef $ nextCalc state
        next $ Unknown u
    run state (TellLogSize name next) = withFile (fName name) ReadMode hFileSize >>= next.fromIntegral
    run state (CheckSumLog name next) = do
        fs <- BL.readFile $ fName name
        next $ hashlazy fs
    run state (ResetLogPosition name next) = do
        modifyIORef' (readPosition state) $ M.adjust (\_ -> 0) name
        next
    run state (SkipForwardLog name dist next) = do
        modifyIORef' (readPosition state) $ M.adjust (+ fromIntegral dist) name
        next
    run state (DeleteLog name next) = do
        modifyIORef' (toAppend state) $ M.delete name
        pos <- readIORef $ positions state
        case M.lookup name pos of
            Nothing -> return ()
            Just x -> DF.forM_ x $ \i -> do
                modifyIORef' (unknown state) $ M.delete undefined
        modifyIORef' (positions state) $ M.delete name
        modifyIORef' (toDelete state) $ ST.insert name
        next
    run state (RenameLog name1 name2 next) = do
        app <- readIORef $ toAppend state
        modifyIORef' (renames state) $ M.insert name1 name2
        case M.lookup name1 app of
            Nothing -> return ()
            Just x -> modifyIORef' (toAppend state) $ M.insert name2 x
        run state (DeleteLog name1 next)
    run state (ReadFromLog _ next) = next $ Left "Worst runner ever."
    run state (AppendLogData name dat next) = do
        ident <- readIORef $ nextCalc state
        when (unresolvedPositions dat > 0) $ do
            modifyIORef' (unknown state) $ M.insert ident $ (0,packLogger dat)
            return ()
        -- TODO: plenty
        modifyIORef' (nextCalc state) succ
        next
    commit state = do
        return ()
