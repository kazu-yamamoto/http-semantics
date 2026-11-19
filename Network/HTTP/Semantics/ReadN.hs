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
import Data.Void (absurd)
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
defaultReadN s ref n = either absurd id <$> defaultReadNWith s ref n retry

-- | 'defaultReadN' which the caller can give up on: 'Left' when the 'STM'
-- action fires before @n@ bytes have come.  The bytes read so far are kept
-- for the next read.  'Right' with the empty string is the end of input.
--
-- @since 0.4.2 (this type since 0.5.0)
defaultReadNWith
    :: Socket -> IORef (Maybe B.ByteString) -> Int -> STM g -> IO (Either g B.ByteString)
defaultReadNWith _ _ 0 _ = return $ Right B.empty
defaultReadNWith s ref n giveUp = do
    mbs <- readIORef ref
    writeIORef ref Nothing
    case mbs of
        Nothing -> loop B.empty
        Just bs
            | B.length bs >= n -> do
                let (bs0, bs1) = B.splitAt n bs
                writeIORef ref $ if B.null bs1 then Nothing else Just bs1
                return $ Right bs0
            | otherwise -> loop bs
  where
    loop bs = do
        r <- N.recvWith s (n - B.length bs) giveUp
        case r of
            Left g -> do
                writeIORef ref $ if B.null bs then Nothing else Just bs
                return $ Left g
            Right bs1
                | B.null bs1 -> return $ Right B.empty
                | otherwise -> do
                    let bs2 = bs `B.append` bs1
                    if B.length bs2 == n
                        then return $ Right bs2
                        else loop bs2
