{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Crypto.Hash.MD5 as MD5
import Control.Exception

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
    , toAppend :: IORef (M.Map LogName (Int64, IORef (BL.ByteString)))
    , nextCalc :: IORef Int
    , positions :: IORef (M.Map LogName (ST.Set Int))
    , toRename :: IORef (ST.Set (LogName, LogName))
    , readPosition :: IORef (M.Map LogName Int64)
    , writePosition :: IORef (M.Map Int (LogName, Int64))
    , unknown :: IORef (M.Map Int (Int64, Logging))
    }

mkState :: IO State
mkState = do
    d <- newIORef ST.empty -- toDelete
    a <- newIORef M.empty -- toAppend
    n <- newIORef 0 -- nextCalc
    p <- newIORef M.empty -- positions
    r <- newIORef ST.empty -- renames
    rp <- newIORef M.empty -- readPosition
    wp <- newIORef M.empty -- writePosition
    uk <- newIORef M.empty -- unknown
    -- Finally, give the state with all of those IORefs.
    return $ State d a n p r rp wp uk

fName :: B.ByteString -> String
fName n = init $ drop 1 $ show n

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
    run state (TellLogWritePosition name next) = readIORef (nextCalc state) >>= next.Unknown
    run state (TellLogSize name next) = do
        catch (getFileSize (fName name) >>= next) (\(e :: IOException) -> next 0)
    run state (CheckSumLog name next) = do
        catch (BL.readFile (fName name) >>= next.MD5.hashlazy) (\(e :: IOException) -> next $ MD5.hash "")
    run state (ResetLogPosition name next) = do
        modifyIORef' (readPosition state) $ M.adjust (\_ -> 0) name
        next
    run state (SkipForwardLog name dist next) = do
        modifyIORef' (readPosition state) $ M.adjust (+ dist) name
        next
    run state (DeleteLog name next) = do
        putStrLn ("Deleting log "
                  ++ fName name
                  )
        modifyIORef' (toAppend state) $ M.delete name
        pos <- readIORef $ positions state
        case M.lookup name pos of
            Nothing -> return ()
            Just x -> DF.forM_ x $ \i -> do
                modifyIORef' (unknown state) $ M.delete i
                modifyIORef' (writePosition state) $ M.delete i
        modifyIORef' (positions state) $ M.delete name
        modifyIORef' (toDelete state) $ ST.insert name
        next
    run state (RenameLog name1 name2 next) = do
        app <- readIORef $ toAppend state
        putStrLn ("Renaming log "
                  ++ fName name1
                  ++ " to "
                  ++ fName name2
                  )
        -- Add renames
        modifyIORef' (toRename state) $ ST.insert (name1, name2)
        -- prevent deleting, if it did happen
        modifyIORef' (toDelete state) $ ST.delete name2
        case M.lookup name1 app of
            Nothing -> return ()
            Just x -> modifyIORef' (toAppend state) $ M.insert name2 x
        run state (DeleteLog name1 next)
    run state (ReadFromLog name next) = do
        -- This is a really bad way, please don't do this.
        bs <- catch (BL.readFile $ fName name) (\(e ::IOException) -> return BL.empty)
        pos <- readIORef $ readPosition state
        let dist = case M.lookup name pos of
                        Nothing -> 0
                        Just x -> x
        let toRead = BL.drop dist bs
        putStrLn ("Attempting to read from "
                  ++ fName name
                  ++ " at "
                  ++ show dist
                  )
        case S.runGetLazyState S.get toRead of
            Left s -> next $ Left s
            Right (decoded, remaining) -> do
                let readSize = BL.length toRead - BL.length remaining
                -- Update the read position
                modifyIORef' (readPosition state) $ M.insertWith (+) name readSize
                next decoded
    run state (AppendLogData name dat next) = do
        ident <- readIORef $ nextCalc state
        let enc = S.encodeLazy dat
        let len = BL.length enc
        writes <- readIORef $ toAppend state
        -- Find out the write position, and the written reference
        (wposition, written) <- case M.lookup name writes of
            Just (s,w) -> do
                appending <- readIORef w
                let appendlen = BL.length appending
                return (s+appendlen,w)
            Nothing -> do
                -- Make sure that we have something there, since next we actually 
                -- operate on appending byte strings and such.
                size <- getFileSize $ fName name
                emp <- newIORef BL.empty
                modifyIORef' (toAppend state) $ M.insert name (0, emp)
                return (size, emp)
        
        when (unresolvedPositions dat > 0) $ do
            -- when there's things to resolve, put it in a queue for later.
            modifyIORef' (unknown state) $ M.insert ident $ (wposition, packLogger dat)
        -- Append the stuff
        modifyIORef' written $ \ws -> BL.append ws enc
        -- Say that we wrote 'here' in case something points to this.
        modifyIORef' (writePosition state) $ M.insert ident (name, wposition)
        -- Increment the number used to identify the next uncommitted entry
        modifyIORef' (nextCalc state) succ
        putStrLn ("Attempting to append: "
                  ++ fName name
                  ++ " "
                  ++ show ident
                  ++ " "
                  ++ show enc
                  )
        next
    commit state = do
        -- Update references
        -- TODO
        -- Rename
        mvs <- readIORef $ toRename state
        DF.forM_ mvs $ \(n1, n2) -> do
            catch
                (renameFile (fName n1) (fName n2))
                (\(e :: IOException) -> return ())
        -- Delete
        dels <- readIORef $ toDelete state
        DF.forM_ dels $ \n -> do
            catch (removeFile (fName n)) (\(e :: IOException) -> return ())
        -- Append
        app <- readIORef $ toAppend state
        DF.forM_ (M.toList app) $ \(name, (_, dat)) -> do
            putStrLn $ "Appending to " ++ fName name
            readIORef dat >>= BL.appendFile (fName name)
        return ()
    getFileSize :: FilePath -> IO Int64
    getFileSize name = catch (BL.readFile name >>= return.BL.length) (\(e :: IOException) -> return 0) 
