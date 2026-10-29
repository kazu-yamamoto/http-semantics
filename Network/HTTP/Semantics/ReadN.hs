module Network.HTTP.Semantics.ReadN (
    -- * Reading n bytes
    ReadN,
    defaultReadN,
    defaultReadNWith,
)
where

import Control.Concurrent.STM
import qualified Data.ByteString as B
import Data.IORef
import Network.Socket
import qualified Network.Socket.ByteString as N

-- | Reading n bytes.
type ReadN = Int -> IO B.ByteString

-- | Naive implementation for readN.
--
-- /NOTE/: This function is intended to be used by a single thread only.
-- (It is probably quite rare anyway to want concurrent reads from the /same/
-- network socket.)
defaultReadN :: Socket -> IORef (Maybe B.ByteString) -> ReadN
defaultReadN s ref = defaultReadNWith s ref retry

-- | 'defaultReadN' which the caller can give up on.  The 'STM' action is
-- waited for beside each receive; when it fires, the read answers with the
-- empty string, which is how 'ReadN' already says that there is nothing more.
--
-- /NOTE/: the bytes read so far are discarded, so this ends a connection
-- rather than pausing one.  The caller cannot tell a give-up from the peer
-- hanging up; whoever fired the 'STM' action knows which it was.
--
-- @since 0.4.2
defaultReadNWith :: Socket -> IORef (Maybe B.ByteString) -> STM a -> ReadN
defaultReadNWith _ _ _ 0 = return B.empty
defaultReadNWith s ref giveUp n = do
    mbs <- readIORef ref
    writeIORef ref Nothing
    case mbs of
        Nothing -> do
            bs <- recv1 n
            if B.null bs
                then return B.empty
                else
                    if B.length bs == n
                        then return bs
                        else loop bs
        Just bs
            | B.length bs == n -> return bs
            | B.length bs > n -> do
                let (bs0, bs1) = B.splitAt n bs
                writeIORef ref (Just bs1)
                return bs0
            | otherwise -> loop bs
  where
    recv1 n' = either (const B.empty) id <$> N.recvWith s n' giveUp
    loop bs = do
        let n' = n - B.length bs
        bs1 <- recv1 n'
        if B.null bs1
            then return B.empty
            else do
                let bs2 = bs `B.append` bs1
                if B.length bs2 == n
                    then return bs2
                    else loop bs2
