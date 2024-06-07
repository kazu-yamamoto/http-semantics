{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.HTTP.Semantics.FillBuf (
    -- * Filling a buffer
    Next (..),
    DynaNext,
    BytesFilled,
    StreamingChunk (..),
    CleanupStream,
    fillBuilderBodyGetNext,
    fillFileBodyGetNext,
    fillStreamBodyGetNext,
) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as B
import Data.ByteString.Internal
import Data.Int (Int64)
import Foreign.Ptr (plusPtr)
import Network.ByteOrder
import Network.HTTP.Semantics.Client

----------------------------------------------------------------

-- type DynaNext = Buffer -> BufferSize -> WindowSize -> IO Next
type DynaNext = Buffer -> BufferSize -> Int -> IO Next

type BytesFilled = Int

data Next
    = Next
        BytesFilled -- payload length
        Bool -- require flushing
        (Maybe DynaNext)

----------------------------------------------------------------

data Leftover
    = LZero
    | LOne B.BufferWriter
    | LTwo ByteString B.BufferWriter

----------------------------------------------------------------

data StreamingChunk
    = -- | Indicate that the stream is finished
      StreamingFinished CleanupStream
    | -- | Flush the stream
      --
      -- This will cause the write buffer to be written to the network socket,
      -- without waiting for more data.
      StreamingFlush
    | -- | Construct a DATA frame
      StreamingBuilder Builder

-- | Action to run prior to terminating the stream
type CleanupStream = IO ()

----------------------------------------------------------------

fillBuilderBodyGetNext :: Builder -> DynaNext
fillBuilderBodyGetNext bb buf siz lim = do
    let room = min siz lim
    (len, signal) <- B.runBuilder bb buf room
    return $ nextForBuilder len signal

fillFileBodyGetNext
    :: PositionRead -> FileOffset -> ByteCount -> IO () -> DynaNext
fillFileBodyGetNext pread start bytecount refresh buf siz lim = do
    let room = min siz lim
    len <- pread start (mini room bytecount) buf
    let len' = fromIntegral len
    return $ nextForFile len' pread (start + len) (bytecount - len) refresh

fillStreamBodyGetNext :: IO (Maybe StreamingChunk) -> DynaNext
fillStreamBodyGetNext takeQ buf siz lim = do
    let room = min siz lim
    (cont, len, reqflush, leftover) <- runStreamBuilder buf room takeQ
    return $ nextForStream cont len reqflush leftover takeQ

----------------------------------------------------------------

fillBufBuilder :: Leftover -> DynaNext
fillBufBuilder leftover buf0 siz0 lim = do
    let room = min siz0 lim
    case leftover of
        LZero -> error "fillBufBuilder: LZero"
        LOne writer -> do
            (len, signal) <- writer buf0 room
            getNext len signal
        LTwo bs writer
            | BS.length bs <= room -> do
                buf1 <- copy buf0 bs
                let len1 = BS.length bs
                (len2, signal) <- writer buf1 (room - len1)
                getNext (len1 + len2) signal
            | otherwise -> do
                let (bs1, bs2) = BS.splitAt room bs
                void $ copy buf0 bs1
                getNext room (B.Chunk bs2 writer)
  where
    getNext l s = return $ nextForBuilder l s

nextForBuilder :: BytesFilled -> B.Next -> Next
nextForBuilder len B.Done =
    Next len True Nothing -- let's flush
nextForBuilder len (B.More _ writer) =
    Next len False $ Just (fillBufBuilder (LOne writer))
nextForBuilder len (B.Chunk bs writer) =
    Next len False $ Just (fillBufBuilder (LTwo bs writer))

----------------------------------------------------------------

runStreamBuilder
    :: Buffer
    -> BufferSize
    -> IO (Maybe StreamingChunk)
    -> IO
        ( Bool -- continue
        , BytesFilled
        , Bool -- require flusing
        , Leftover
        )
runStreamBuilder buf0 room0 takeQ = loop buf0 room0 0
  where
    loop buf room total = do
        mbuilder <- takeQ
        case mbuilder of
            Nothing -> return (True, total, False, LZero)
            Just (StreamingBuilder builder) -> do
                (len, signal) <- B.runBuilder builder buf room
                let total' = total + len
                case signal of
                    B.Done -> loop (buf `plusPtr` len) (room - len) total'
                    B.More _ writer -> return (True, total', False, LOne writer)
                    B.Chunk bs writer -> return (True, total', False, LTwo bs writer)
            Just StreamingFlush -> return (True, total, True, LZero)
            Just (StreamingFinished dec) -> do
                dec
                return (False, total, True, LZero)

fillBufStream :: Leftover -> IO (Maybe StreamingChunk) -> DynaNext
fillBufStream leftover0 takeQ buf0 siz0 lim0 = do
    let room0 = min siz0 lim0
    case leftover0 of
        LZero -> do
            (cont, len, reqflush, leftover) <- runStreamBuilder buf0 room0 takeQ
            getNext cont len reqflush leftover
        LOne writer -> write writer buf0 room0 0
        LTwo bs writer
            | BS.length bs <= room0 -> do
                buf1 <- copy buf0 bs
                let len = BS.length bs
                write writer buf1 (room0 - len) len
            | otherwise -> do
                let (bs1, bs2) = BS.splitAt room0 bs
                void $ copy buf0 bs1
                getNext True room0 False $ LTwo bs2 writer
  where
    getNext :: Bool -> BytesFilled -> Bool -> Leftover -> IO Next
    getNext cont len reqflush l = return $ nextForStream cont len reqflush l takeQ

    write
        :: (Buffer -> BufferSize -> IO (Int, B.Next))
        -> Buffer
        -> BufferSize
        -> Int
        -> IO Next
    write writer1 buf room sofar = do
        (len, signal) <- writer1 buf room
        case signal of
            B.Done -> do
                (cont, extra, reqflush, leftover) <-
                    runStreamBuilder (buf `plusPtr` len) (room - len) takeQ
                let total = sofar + len + extra
                getNext cont total reqflush leftover
            B.More _ writer -> do
                let total = sofar + len
                getNext True total False $ LOne writer
            B.Chunk bs writer -> do
                let total = sofar + len
                getNext True total False $ LTwo bs writer

nextForStream
    :: Bool
    -> BytesFilled
    -> Bool
    -> Leftover
    -> IO (Maybe StreamingChunk)
    -> Next
nextForStream False len reqflush _ _ = Next len reqflush Nothing
nextForStream True len reqflush leftOrZero takeQ =
    Next len reqflush $ Just (fillBufStream leftOrZero takeQ)

----------------------------------------------------------------

fillBufFile :: PositionRead -> FileOffset -> ByteCount -> IO () -> DynaNext
fillBufFile pread start bytes refresh buf siz lim = do
    let room = min siz lim
    len <- pread start (mini room bytes) buf
    refresh
    let len' = fromIntegral len
    return $ nextForFile len' pread (start + len) (bytes - len) refresh

nextForFile
    :: BytesFilled -> PositionRead -> FileOffset -> ByteCount -> IO () -> Next
nextForFile 0 _ _ _ _ = Next 0 True Nothing -- let's flush
nextForFile len _ _ 0 _ = Next len False Nothing
nextForFile len pread start bytes refresh =
    Next len False $ Just $ fillBufFile pread start bytes refresh

{-# INLINE mini #-}
mini :: Int -> Int64 -> Int64
mini i n
    | fromIntegral i < n = fromIntegral i
    | otherwise = n
