
module Solar.Continuum.Types where

import qualified Data.Serialize as S
import qualified Data.ByteString as B
import Data.Int
import Control.Applicative

type LogName = B.ByteString

-- | Known positions are meant to be persisted,
-- but data may go through intermediary stages as the positions are finalized
data LogPos
    = Known Int64
    | Unknown Int
    deriving (Eq, Show)

-- | Shortcut for what 'S.Serialize' has when parsing
-- Only to be used when reading from a log, not when getting
-- a message.
type TryRead a = Either String a

-- | Gets and stores the position within a log as a 64 bit int.
-- If something is ever persisted that should not be, look for the magic number,
-- @0xFEED0B0BDEADBEEF@
instance S.Serialize LogPos where
    put p = case p of
        Known v -> S.putWord64host $ fromIntegral v
        Unknown _ -> S.putWord64host 0xFEED0B0BDEADBEEF
        -- The above: Filler for now. Pointless to store the internal identifier.
    get = Known <$> fromIntegral <$> S.getWord64host

-- | A loggable and serializeable data type should implement this.
-- A note to implementors, make sure to use least-fixed-point
-- to detect whether or not the collection to persist is not fully resolveable.
class (S.Serialize a) => Loggable a where
    resolvePositions :: a -> (Int -> Maybe Int64) -> a
    unresolvedPositions :: a -> Int