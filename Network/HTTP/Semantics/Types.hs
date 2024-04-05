{-# LANGUAGE RankNTypes #-}

module Network.HTTP.Semantics.Types where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.IORef
import Data.Int (Int64)
import qualified Network.HTTP.Types as H

import Network.HTTP.Semantics.Header

----------------------------------------------------------------

-- | "http" or "https".
type Scheme = ByteString

-- | Authority.
type Authority = String

-- | Path.
type Path = ByteString

----------------------------------------------------------------

type InpBody = IO ByteString

data OutBody
    = OutBodyNone
    | -- | Streaming body takes a write action and a flush action.
      OutBodyStreaming ((Builder -> IO ()) -> IO () -> IO ())
    | -- | Like 'OutBodyStreaming', but with a callback to unmask expections
      --
      -- This is used in the client: we spawn the new thread for the request body
      -- with exceptions masked, and provide the body of 'OutBodyStreamingUnmask'
      -- with a callback to unmask them again (typically after installing an exception
      -- handler).
      --
      -- We do /NOT/ support this in the server, as here the scope of the thread
      -- that is spawned for the server is the entire handler, not just the response
      -- streaming body.
      --
      -- TODO: The analogous change for the server-side would be to provide a similar
      -- @unmask@ callback as the first argument in the 'Server' type alias.
      OutBodyStreamingUnmask
        ((forall x. IO x -> IO x) -> (Builder -> IO ()) -> IO () -> IO ())
    | OutBodyBuilder Builder
    | OutBodyFile FileSpec

-- | Input object
data InpObj = InpObj
    { inpObjHeaders :: HeaderTable
    -- ^ Accessor for headers.
    , inpObjBodySize :: Maybe Int
    -- ^ Accessor for body length specified in content-length:.
    , inpObjBody :: InpBody
    -- ^ Accessor for body.
    , inpObjTrailers :: IORef (Maybe HeaderTable)
    -- ^ Accessor for trailers.
    }

instance Show InpObj where
    show (InpObj (thl, _) _ _body _tref) = show thl

-- | Output object
data OutObj = OutObj
    { outObjHeaders :: [H.Header]
    -- ^ Accessor for header.
    , outObjBody :: OutBody
    -- ^ Accessor for outObj body.
    , outObjTrailers :: TrailersMaker
    -- ^ Accessor for trailers maker.
    }

instance Show OutObj where
    show (OutObj hdr _ _) = show hdr

-- | Trailers maker. A chunks of the response body is passed
--   with 'Just'. The maker should update internal state
--   with the 'ByteString' and return the next trailers maker.
--   When response body reaches its end,
--   'Nothing' is passed and the maker should generate
--   trailers. An example:
--
--   > {-# LANGUAGE BangPatterns #-}
--   > import Data.ByteString (ByteString)
--   > import qualified Data.ByteString.Char8 as C8
--   > import Crypto.Hash (Context, SHA1) -- cryptonite
--   > import qualified Crypto.Hash as CH
--   >
--   > -- Strictness is important for Context.
--   > trailersMaker :: Context SHA1 -> Maybe ByteString -> IO NextTrailersMaker
--   > trailersMaker ctx Nothing = return $ Trailers [("X-SHA1", sha1)]
--   >   where
--   >     !sha1 = C8.pack $ show $ CH.hashFinalize ctx
--   > trailersMaker ctx (Just bs) = return $ NextTrailersMaker $ trailersMaker ctx'
--   >   where
--   >     !ctx' = CH.hashUpdate ctx bs
--
--   Usage example:
--
--   > let h2rsp = responseFile ...
--   >     maker = trailersMaker (CH.hashInit :: Context SHA1)
--   >     h2rsp' = setResponseTrailersMaker h2rsp maker
type TrailersMaker = Maybe ByteString -> IO NextTrailersMaker

-- | TrailersMake to create no trailers.
defaultTrailersMaker :: TrailersMaker
defaultTrailersMaker Nothing = return $ Trailers []
defaultTrailersMaker _ = return $ NextTrailersMaker defaultTrailersMaker

-- | Either the next trailers maker or final trailers.
data NextTrailersMaker
    = NextTrailersMaker TrailersMaker
    | Trailers [H.Header]

----------------------------------------------------------------

-- | Offset for file.
type FileOffset = Int64

-- | How many bytes to read
type ByteCount = Int64

-- | File specification.
data FileSpec = FileSpec FilePath FileOffset ByteCount deriving (Eq, Show)
