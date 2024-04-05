{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Semantics.Server (
    -- * HTTP\/2 server
    Server,

    -- * Request
    Request,

    -- ** Accessing request
    requestMethod,
    requestPath,
    requestAuthority,
    requestScheme,
    requestHeaders,
    requestBodySize,
    getRequestBodyChunk,
    getRequestTrailers,

    -- * Aux
    Aux,
    auxTimeHandle,
    auxMySockAddr,
    auxPeerSockAddr,

    -- * Response
    Response,

    -- ** Creating response
    responseNoBody,
    responseFile,
    responseStreaming,
    responseBuilder,

    -- ** Accessing response
    responseBodySize,

    -- ** Trailers maker
    TrailersMaker,
    NextTrailersMaker (..),
    defaultTrailersMaker,
    setResponseTrailersMaker,

    -- * Push promise
    PushPromise,
    pushPromise,
    promiseRequestPath,
    promiseResponse,

    -- * Types
    Path,
    Authority,
    Scheme,
    FileSpec (..),
    FileOffset,
    ByteCount,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.UTF8 as UTF8
import Data.IORef
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)
import qualified System.TimeManager as T

import Network.HTTP.Semantics.Internal
import Network.HTTP.Semantics.Status
import Network.HTTP.Semantics.Token

----------------------------------------------------------------

-- | Server type. Server takes a HTTP request, should
--   generate a HTTP response and push promises, then
--   should give them to the sending function.
--   The sending function would throw exceptions so that
--   they can be logged.
type Server = Request -> Aux -> (Response -> [PushPromise] -> IO ()) -> IO ()

-- | Request from client.
newtype Request = Request InpObj deriving (Show)

-- | Response from server.
newtype Response = Response OutObj deriving (Show)

-- | HTTP/2 push promise or sever push.
--   Pseudo REQUEST headers in push promise is automatically generated.
--   Then, a server push is sent according to 'promiseResponse'.
data PushPromise = PushPromise
    { promiseRequestPath :: ByteString
    -- ^ Accessor for a URL path in a push promise (a virtual request from a server).
    --   E.g. \"\/style\/default.css\".
    , promiseResponse :: Response
    -- ^ Accessor for response actually pushed from a server.
    }

-- | Additional information.
data Aux = Aux
    { auxTimeHandle :: T.Handle
    -- ^ Time handle for the worker processing this request and response.
    , auxMySockAddr :: SockAddr
    -- ^ Local socket address copied from 'Config'.
    , auxPeerSockAddr :: SockAddr
    -- ^ Remove socket address copied from 'Config'.
    }

----------------------------------------------------------------

-- | Getting the method from a request.
requestMethod :: Request -> Maybe H.Method
requestMethod (Request req) = getHeaderValue tokenMethod vt
  where
    (_, vt) = inpObjHeaders req

-- | Getting the path from a request.
requestPath :: Request -> Maybe Path
requestPath (Request req) = getHeaderValue tokenPath vt
  where
    (_, vt) = inpObjHeaders req

-- | Getting the authority from a request.
requestAuthority :: Request -> Maybe Authority
requestAuthority (Request req) = UTF8.toString <$> getHeaderValue tokenAuthority vt
  where
    (_, vt) = inpObjHeaders req

-- | Getting the scheme from a request.
requestScheme :: Request -> Maybe Scheme
requestScheme (Request req) = getHeaderValue tokenScheme vt
  where
    (_, vt) = inpObjHeaders req

-- | Getting the headers from a request.
requestHeaders :: Request -> HeaderTable
requestHeaders (Request req) = inpObjHeaders req

-- | Getting the body size from a request.
requestBodySize :: Request -> Maybe Int
requestBodySize (Request req) = inpObjBodySize req

-- | Reading a chunk of the request body.
--   An empty 'ByteString' returned when finished.
getRequestBodyChunk :: Request -> IO ByteString
getRequestBodyChunk (Request req) = inpObjBody req

-- | Reading request trailers.
--   This function must be called after 'getRequestBodyChunk'
--   returns an empty.
getRequestTrailers :: Request -> IO (Maybe HeaderTable)
getRequestTrailers (Request req) = readIORef (inpObjTrailers req)

----------------------------------------------------------------

-- | Creating response without body.
responseNoBody :: H.Status -> H.ResponseHeaders -> Response
responseNoBody st hdr = Response $ OutObj hdr' OutBodyNone defaultTrailersMaker
  where
    hdr' = setStatus st hdr

-- | Creating response with file.
responseFile :: H.Status -> H.ResponseHeaders -> FileSpec -> Response
responseFile st hdr fileSpec = Response $ OutObj hdr' (OutBodyFile fileSpec) defaultTrailersMaker
  where
    hdr' = setStatus st hdr

-- | Creating response with builder.
responseBuilder :: H.Status -> H.ResponseHeaders -> Builder -> Response
responseBuilder st hdr builder = Response $ OutObj hdr' (OutBodyBuilder builder) defaultTrailersMaker
  where
    hdr' = setStatus st hdr

-- | Creating response with streaming.
responseStreaming
    :: H.Status
    -> H.ResponseHeaders
    -> ((Builder -> IO ()) -> IO () -> IO ())
    -> Response
responseStreaming st hdr strmbdy = Response $ OutObj hdr' (OutBodyStreaming strmbdy) defaultTrailersMaker
  where
    hdr' = setStatus st hdr

----------------------------------------------------------------

-- | Getter for response body size. This value is available for file body.
responseBodySize :: Response -> Maybe Int
responseBodySize (Response (OutObj _ (OutBodyFile (FileSpec _ _ len)) _)) = Just (fromIntegral len)
responseBodySize _ = Nothing

-- | Setting 'TrailersMaker' to 'Response'.
setResponseTrailersMaker :: Response -> TrailersMaker -> Response
setResponseTrailersMaker (Response rsp) tm = Response rsp{outObjTrailers = tm}

----------------------------------------------------------------

-- | Creating push promise.
--   The third argument is traditional, not used.
pushPromise :: ByteString -> Response -> Int -> PushPromise
pushPromise path rsp _ = PushPromise path rsp