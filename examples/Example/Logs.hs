{-# LANGUAGE OverloadedStrings #-}

module Example.Logs where

import Solar.Continuum.Log
import Solar.Continuum.Types

import Data.Word
import Control.Applicative
import Control.Monad

import Data.Serialize

-- Begin Data stuff, you'd probably have this in another module
-- It is recommended to use sum-types instead of 
data Blah = Blah Word8
instance Serialize Blah where
    put (Blah v) = putWord8 $ v + 64
    get = getWord8 >>= return.Blah.(\v -> v - 64)
instance Loggable Blah where
    resolvePositions a _ = a
    unresolvedPositions _ = 0
-- End data stuff.

logName1 :: LogName
logName1 = "blah"
logName2 :: LogName
logName2 = "blah2"
logName3 :: LogName
logName3 = "checksums"

-- Here's where you can do things, although they probably don't mean much.
exampleLog1 :: Log ()
exampleLog1 = do
    -- Write things
    forM_ [1..10] $ \num -> do
        let item = Blah num
        appendLogData logName1 item
        -- Write to another, because we can
        appendLogData logName2 item
        -- But double it so that we can have different checksums
        appendLogData logName2 item

exampleLog2 :: Log Bool
exampleLog2 = do
    -- Compare logs or something
    checksum1 <- checkSumLog logName1
    -- You can also get checksum from a log by reading.
    checksum2 <- checkSumLog logName2
    return $ checksum1 == checksum2


exampleLog :: Log ()
exampleLog = do
    deleteLog "blah"
    deleteLog "blah2"
    -- We can compose log actions
    exampleLog1
    -- Though doing the following will not really do much (if blah and blah2 are empty)
    -- Since checksums and such can only be obtained after the log has been committed
    -- to the runner.
    same <- exampleLog2
    -- It should always be true in this case, since we deleted them, and ideally
    -- two empty byte strings are equal.
    if same
    then appendLogData logName2 $ Blah 1
    else appendLogData logName2 $ Blah 0









