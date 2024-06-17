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
type DynaNext = Buffer -> Int -> IO Next

type BytesFilled = Int

data Next
    = Next
        BytesFilled -- payload length
        Bool -- require flushing
        (Maybe DynaNext)

----------------------------------------------------------------

data StreamingChunk
    = -- | Indicate that the stream is finished
      StreamingFinished CleanupStream
    | -- | Flush the stream
      --
      -- This will cause the write buffer to be written to the network socket,
      -- without waiting for more data.
      StreamingFlush
    | -- | Construct a DATA frame, optionally terminating the stream
      --
      -- The optional 'CleanupStream' argument can be used to ensure that the
      -- final DATA frame in the stream is marked as end-of-stream, as opposed
      -- to using a separate, /empty/, data frame with this flag set.
      StreamingBuilder Builder (Maybe CleanupStream)

-- | Action to run prior to terminating the stream
type CleanupStream = IO ()

----------------------------------------------------------------

fillBuilderBodyGetNext :: Builder -> DynaNext
fillBuilderBodyGetNext bb buf room = do
    (len, signal) <- B.runBuilder bb buf room
    return $ nextForBuilder len signal

fillFileBodyGetNext
    :: PositionRead -> FileOffset -> ByteCount -> IO () -> DynaNext
fillFileBodyGetNext pread start bytecount refresh buf room = do
    len <- pread start (mini room bytecount) buf
    let len' = fromIntegral len
    return $ nextForFile len' pread (start + len) (bytecount - len) refresh

fillStreamBodyGetNext :: IO (Maybe StreamingChunk) -> DynaNext
fillStreamBodyGetNext takeQ = loop 0
  where
    loop :: NextWithTotal
    loop total buf room = do
        mChunk <- takeQ
        case mChunk of
            Just chunk -> runStreamingChunk chunk loop total buf room
            Nothing -> return $ Next total False (Just $ loop 0)

----------------------------------------------------------------

fillBufBuilderOne :: B.BufferWriter -> DynaNext
fillBufBuilderOne writer buf0 room = do
    (len, signal) <- writer buf0 room
    return $ nextForBuilder len signal

fillBufBuilderTwo :: ByteString -> B.BufferWriter -> DynaNext
fillBufBuilderTwo bs writer buf0 room
    | BS.length bs <= room = do
        buf1 <- copy buf0 bs
        let len1 = BS.length bs
        (len2, signal) <- writer buf1 (room - len1)
        return $ nextForBuilder (len1 + len2) signal
    | otherwise = do
        let (bs1, bs2) = BS.splitAt room bs
        void $ copy buf0 bs1
        return $ nextForBuilder room (B.Chunk bs2 writer)

nextForBuilder :: BytesFilled -> B.Next -> Next
nextForBuilder len B.Done =
    Next len True Nothing -- let's flush
nextForBuilder len (B.More _ writer) =
    Next len False $ Just (fillBufBuilderOne writer)
nextForBuilder len (B.Chunk bs writer) =
    Next len False $ Just (fillBufBuilderTwo bs writer)

----------------------------------------------------------------

-- | Like 'DynaNext', but with additional argument indicating total bytes written
type NextWithTotal = Int -> DynaNext

-- | Run the chunk, then continue as specified, unless streaming is finished
runStreamingChunk :: StreamingChunk -> NextWithTotal -> NextWithTotal
runStreamingChunk chunk next =
    case chunk of
        StreamingFinished dec -> finished dec
        StreamingFlush -> flush
        StreamingBuilder builder Nothing -> runStreamingBuilder builder next
        StreamingBuilder builder (Just dec) -> runStreamingBuilder builder (finished dec)
  where
    finished :: CleanupStream -> NextWithTotal
    finished dec = \total _buf _room -> do
        dec
        return $ Next total True Nothing

    flush :: NextWithTotal
    flush = \total _buf _room -> do
        return $ Next total True (Just $ next 0)

-- | Run 'Builder' until completion, then continue as specified
runStreamingBuilder :: Builder -> NextWithTotal -> NextWithTotal
runStreamingBuilder builder next = \total buf room -> do
    writeResult <- B.runBuilder builder buf room
    ranWriter writeResult total buf room
  where
    ranWriter :: (Int, B.Next) -> NextWithTotal
    ranWriter (len, signal) = \total buf room -> do
        let total' = total + len
        case signal of
            B.Done ->
                next total' (buf `plusPtr` len) (room - len)
            B.More minReq writer ->
                return $ Next total' False (Just $ goMore (Just minReq) writer 0)
            B.Chunk bs writer ->
                return $ Next total' False (Just $ goChunk bs writer 0)

    goMore :: Maybe Int -> B.BufferWriter -> NextWithTotal
    goMore mMinReq writer = \total buf room -> do
        let enoughRoom = maybe True (room >=) mMinReq
        if enoughRoom
            then do
              writeResult <- writer buf room
              ranWriter writeResult total buf room
            else do
              return $ Next total True (Just $ goMore mMinReq writer 0)

    goChunk :: ByteString -> B.BufferWriter -> NextWithTotal
    goChunk bs writer = \total buf room ->
        if BS.length bs <= room
            then do
                buf' <- copy buf bs
                let len = BS.length bs
                goMore Nothing writer (total + len) buf' (room - len)
            else do
                let (bs1, bs2) = BS.splitAt room bs
                void $ copy buf bs1
                return $ Next (total + room) False (Just $ goChunk bs2 writer 0)

----------------------------------------------------------------

fillBufFile :: PositionRead -> FileOffset -> ByteCount -> IO () -> DynaNext
fillBufFile pread start bytes refresh buf room = do
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
