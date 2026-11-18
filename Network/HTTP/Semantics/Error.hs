-- | Errors which end an HTTP connection or stream abnormally, the same
--   over HTTP\/2 and HTTP\/3.
--
--   Only what ended abnormally is here.  A connection closed with no
--   error, stopped, or given up on by the user is a normal end: it is
--   returned, not thrown.
--
--   @since 0.5.0
module Network.HTTP.Semantics.Error (
    -- * Errors
    HTTPError (..),
    StreamId,
    ReasonPhrase,
    isRetryable,

    -- * Error codes
    HTTPErrorCode (..),
    ErrorKind (..),
    ErrorSpace (..),
    http2ErrorCode,
    http3ErrorCode,
    quicTransportErrorCode,
) where

import qualified Control.Exception as E
import Data.ByteString.Short (ShortByteString)
import Data.Int (Int64)
import Data.Word (Word64)

-- | A stream identifier, of HTTP\/2 or of QUIC.
type StreamId = Int64

-- | Why, in words, for the peer or a log.
type ReasonPhrase = ShortByteString

-- | An HTTP connection or stream which ended abnormally.
data HTTPError
    = -- | The peer is gone, or does not send what it owes.
      ConnectionIsTimeout
    | -- | The transport broke: TCP reset, or a QUIC stateless reset.
      ConnectionIsReset
    | -- | The connection closed normally while this was waiting.
      ConnectionIsClosed
    | -- | The connection closed before the peer took this request: it
      --   has not been processed and may be sent again.
      RequestIsUnprocessed StreamId
    | ConnectionErrorIsReceived HTTPErrorCode ReasonPhrase
    | ConnectionErrorIsSent HTTPErrorCode ReasonPhrase
    | StreamErrorIsReceived HTTPErrorCode StreamId
    | StreamErrorIsSent HTTPErrorCode StreamId ReasonPhrase
    | BadThingHappen E.SomeException
    deriving (Show)

instance E.Exception HTTPError

-- | Whether the request was not processed by the peer, and may be sent
--   again (RFC 9113, section 8.7; RFC 9114, section 4.1.1).
isRetryable :: HTTPError -> Bool
isRetryable (RequestIsUnprocessed _) = True
isRetryable (StreamErrorIsReceived code _) = errorKind code == Refused
isRetryable _ = False

-- | An error code of HTTP\/2, of HTTP\/3 or of QUIC, with what it means.
data HTTPErrorCode = HTTPErrorCode
    { errorKind :: ErrorKind
    , errorSpace :: ErrorSpace
    , errorValue :: Word64
    -- ^ The code itself, for a log.
    }
    deriving (Eq, Show)

-- | What an error code means, after RFC 9114, appendix A.4.1.
data ErrorKind
    = NotAnError
    | ProtocolViolation
    | CompressionFailure
    | InternalFailure
    | -- | Not processed: the request may be sent again.
      Refused
    | Cancelled
    | Incomplete
    | MalformedMessage
    | ConnectFailure
    | ExcessiveLoad
    | VersionFallback
    | SecurityFailure
    | TransportFailure
    | UnknownCode
    deriving (Eq, Show)

-- | Whose code it is: they overlap.
data ErrorSpace = HTTP2 | HTTP3 | QUICTransport
    deriving (Eq, Show)

-- | An HTTP\/2 error code (RFC 9113, section 7).
http2ErrorCode :: Word64 -> HTTPErrorCode
http2ErrorCode w = HTTPErrorCode kind HTTP2 w
  where
    kind = case w of
        0x0 -> NotAnError
        0x1 -> ProtocolViolation
        0x2 -> InternalFailure
        0x3 -> ProtocolViolation -- FLOW_CONTROL_ERROR
        0x4 -> ProtocolViolation -- SETTINGS_TIMEOUT
        0x5 -> ProtocolViolation -- STREAM_CLOSED
        0x6 -> ProtocolViolation -- FRAME_SIZE_ERROR
        0x7 -> Refused
        0x8 -> Cancelled
        0x9 -> CompressionFailure
        0xa -> ConnectFailure
        0xb -> ExcessiveLoad -- ENHANCE_YOUR_CALM
        0xc -> SecurityFailure
        0xd -> VersionFallback -- HTTP_1_1_REQUIRED
        _ -> UnknownCode

-- | An HTTP\/3 or QPACK error code (RFC 9114, section 8.1; RFC 9204,
--   section 6).
http3ErrorCode :: Word64 -> HTTPErrorCode
http3ErrorCode w = HTTPErrorCode kind HTTP3 w
  where
    kind = case w of
        0x100 -> NotAnError
        0x101 -> ProtocolViolation
        0x102 -> InternalFailure
        0x103 -> ProtocolViolation -- H3_STREAM_CREATION_ERROR
        0x104 -> ProtocolViolation -- H3_CLOSED_CRITICAL_STREAM
        0x105 -> ProtocolViolation -- H3_FRAME_UNEXPECTED
        0x106 -> ProtocolViolation -- H3_FRAME_ERROR
        0x107 -> ExcessiveLoad
        0x108 -> ProtocolViolation -- H3_ID_ERROR
        0x109 -> ProtocolViolation -- H3_SETTINGS_ERROR
        0x10a -> ProtocolViolation -- H3_MISSING_SETTINGS
        0x10b -> Refused -- H3_REQUEST_REJECTED
        0x10c -> Cancelled
        0x10d -> Incomplete
        0x10e -> MalformedMessage
        0x10f -> ConnectFailure
        0x110 -> VersionFallback
        0x200 -> CompressionFailure -- QPACK_DECOMPRESSION_FAILED
        0x201 -> CompressionFailure -- QPACK_ENCODER_STREAM_ERROR
        0x202 -> CompressionFailure -- QPACK_DECODER_STREAM_ERROR
        _ -> UnknownCode

-- | A QUIC transport error code (RFC 9000, section 20.1): the
--   connection failed below HTTP.
quicTransportErrorCode :: Word64 -> HTTPErrorCode
quicTransportErrorCode w = HTTPErrorCode kind QUICTransport w
  where
    kind
        | w == 0 = NotAnError
        | otherwise = TransportFailure
