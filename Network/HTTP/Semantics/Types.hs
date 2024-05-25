{-# LANGUAGE RankNTypes #-}

module Network.HTTP.Semantics.Types (
    -- * Request/response as input
    InpObj (..),
    InpBody,

    -- * Request/response as output
    OutObj (..),
    OutBody (..),

    -- * Trailers maker
    TrailersMaker,
    defaultTrailersMaker,
    NextTrailersMaker (..),

    -- * File spec
    FileOffset,
    ByteCount,
    FileSpec (..),

    -- * Types
    Scheme,
    Authority,
    Path,
) where

import Data.ByteString.Builder (Builder)
import Data.IORef
import Data.Int (Int64)
import Network.ByteOrder
import qualified Network.HTTP.Types as H

import Network.HTTP.Semantics.Header
import Network.HTTP.Semantics.Trailer

----------------------------------------------------------------

-- | "http" or "https".
type Scheme = ByteString

-- | Authority.
type Authority = String

-- | Path.
type Path = ByteString

----------------------------------------------------------------

type InpBody = IO (ByteString, Bool)

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
    { inpObjHeaders :: TokenHeaderTable
    -- ^ Accessor for headers.
    , inpObjBodySize :: Maybe Int
    -- ^ Accessor for body length specified in content-length:.
    , inpObjBody :: InpBody
    -- ^ Accessor for body.
    , inpObjTrailers :: IORef (Maybe TokenHeaderTable)
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

----------------------------------------------------------------

-- | Offset for file.
type FileOffset = Int64

-- | How many bytes to read
type ByteCount = Int64

-- | File specification.
data FileSpec = FileSpec FilePath FileOffset ByteCount deriving (Eq, Show)
