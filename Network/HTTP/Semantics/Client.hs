{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.HTTP.Semantics.Client (
    -- * HTTP client
    Client,
    SendRequest,

    -- * Request
    Request,

    -- * Creating request
    requestNoBody,
    requestFile,
    requestStreaming,
    requestStreamingUnmask,
    requestBuilder,

    -- ** Generalized streaming interface
    OutBodyIface (..),
    StreamTerminated (..),
    requestStreamingIface,

    -- ** Trailers maker
    TrailersMaker,
    NextTrailersMaker (..),
    defaultTrailersMaker,
    setRequestTrailersMaker,

    -- ** Giving up
    sendRequestWith,

    -- * Response
    Response,

    -- ** Accessing response
    responseStatus,
    responseHeaders,
    responseBodySize,
    getResponseBodyChunk,
    getResponseBodyChunk',
    getResponseTrailers,

    -- * Aux
    Aux,
    defaultAux,
    auxPossibleClientStreams,
    auxSendPing,

    -- * Types
    Scheme,
    Authority,
    Method,
    Path,
    FileSpec (..),
    FileOffset,
    ByteCount,
    module Network.HTTP.Semantics.ReadN,
    module Network.HTTP.Semantics.File,
) where

import Control.Concurrent.STM (STM, atomically, orElse, retry)
import Control.Monad (void)
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.IORef (readIORef)
import Network.HTTP.Types (Method, RequestHeaders, Status)

import Network.HTTP.Semantics
import Network.HTTP.Semantics.Client.Internal
import Network.HTTP.Semantics.File
import Network.HTTP.Semantics.ReadN
import Network.HTTP.Semantics.Status

----------------------------------------------------------------

-- | Send a request and receive its response.
type SendRequest = forall r. Request -> (Response -> IO r) -> IO r

-- | Client type.
type Client a = SendRequest -> Aux -> IO a

----------------------------------------------------------------

-- | Creating request without body.
requestNoBody :: Method -> Path -> RequestHeaders -> Request
requestNoBody m p hdr = Request $ OutObj hdr' OutBodyNone defaultTrailersMaker retry
  where
    hdr' = addHeaders m p hdr

-- | Creating request with file.
requestFile :: Method -> Path -> RequestHeaders -> FileSpec -> Request
requestFile m p hdr fileSpec = Request $ OutObj hdr' (OutBodyFile fileSpec) defaultTrailersMaker retry
  where
    hdr' = addHeaders m p hdr

-- | Creating request with builder.
requestBuilder :: Method -> Path -> RequestHeaders -> Builder -> Request
requestBuilder m p hdr builder = Request $ OutObj hdr' (OutBodyBuilder builder) defaultTrailersMaker retry
  where
    hdr' = addHeaders m p hdr

-- | Creating request with streaming.
requestStreaming
    :: Method
    -> Path
    -> RequestHeaders
    -> ((Builder -> IO ()) -> IO () -> IO ())
    -> Request
requestStreaming m p hdr strmbdy = Request $ OutObj hdr' (OutBodyStreaming strmbdy) defaultTrailersMaker retry
  where
    hdr' = addHeaders m p hdr
-- Not inlined: GHC 9.12.4 and 9.14.1 may store a never-returning body
-- unevaluated in the strict field, read as the first constructor (ghc#27857).
{-# NOINLINE requestStreaming #-}

-- | Like 'requestStreaming', but run the action with exceptions masked
requestStreamingUnmask
    :: Method
    -> Path
    -> RequestHeaders
    -> ((forall x. IO x -> IO x) -> (Builder -> IO ()) -> IO () -> IO ())
    -> Request
requestStreamingUnmask m p hdr strmbdy = requestStreamingIface m p hdr $ \iface ->
    strmbdy (outBodyUnmask iface) (outBodyPush iface) (outBodyFlush iface)

-- | Generalized version of 'requestStreaming',
requestStreamingIface
    :: Method
    -> Path
    -> RequestHeaders
    -> (OutBodyIface -> IO ())
    -> Request
requestStreamingIface m p hdr strmbdy = Request $ OutObj hdr' (OutBodyStreamingIface strmbdy) defaultTrailersMaker retry
  where
    hdr' = addHeaders m p hdr
-- Not inlined: GHC 9.12.4 and 9.14.1 may store a never-returning body
-- unevaluated in the strict field, read as the first constructor (ghc#27857).
{-# NOINLINE requestStreamingIface #-}

addHeaders :: Method -> Path -> RequestHeaders -> RequestHeaders
addHeaders m p hdr = (":method", m) : (":path", p) : hdr

-- | Setting 'TrailersMaker' to 'Response'.
setRequestTrailersMaker :: Request -> TrailersMaker -> Request
setRequestTrailersMaker (Request req) tm = Request req{outObjTrailers = tm}

-- | Sending a request and handling its response as 'SendRequest' does,
--   giving up on it once the 'STM' action, the last argument,
--   succeeds: while its stream is opened, while it is sent,
--   while the response is waited for, and while the handler reads the
--   body.  The stream is reset and what the action gave is returned as
--   'Left'.  A request already done by then is not given up on.  The
--   action is run wherever the request waits, and once more for what it
--   gave: it should go on succeeding once it has, as a deadline does.
--
-- @since 0.5.0
sendRequestWith
    :: SendRequest -> Request -> (Response -> IO r) -> STM g -> IO (Either g r)
sendRequestWith send req handler giveUp = do
    r <- E.try $ send (setRequestGiveUp req $ void giveUp) handler
    case r of
        Right x -> return $ Right x
        Left RequestGivenUp -> do
            mg <- atomically $ (Just <$> giveUp) `orElse` return Nothing
            maybe (E.throwIO RequestGivenUp) (return . Left) mg

----------------------------------------------------------------

-- | Getting the status of a response.
responseStatus :: Response -> Maybe Status
responseStatus (Response rsp) = getStatus $ inpObjHeaders rsp

-- | Getting the headers from a response.
responseHeaders :: Response -> TokenHeaderTable
responseHeaders (Response rsp) = inpObjHeaders rsp

-- | Getting the body size from a response.
responseBodySize :: Response -> Maybe Int
responseBodySize (Response rsp) = inpObjBodySize rsp

-- | Reading a chunk of the response body.
--   An empty 'ByteString' returned when finished.
getResponseBodyChunk :: Response -> IO ByteString
getResponseBodyChunk = fmap fst . getResponseBodyChunk'

-- | Generalization of 'getResponseBodyChunk' which also returns if the 'ByteString' is the final one
getResponseBodyChunk' :: Response -> IO (ByteString, Bool)
getResponseBodyChunk' (Response rsp) = inpObjBody rsp

-- | Reading response trailers.
--   This function must be called after 'getResponseBodyChunk'
--   returns an empty.
getResponseTrailers :: Response -> IO (Maybe TokenHeaderTable)
getResponseTrailers (Response rsp) = readIORef (inpObjTrailers rsp)
