module Network.HTTP.Semantics.Trailer where

import Network.ByteOrder
import qualified Network.HTTP.Types as H

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
--   > trailersMaker :: Context SHA1 -> TrailersMaker
--   > trailersMaker ctx = TrailersMaker [("X-SHA1", sha1)] next
--   >   where
--   >     !sha1 = C8.pack $ show $ CH.hashFinalize ctx
--   >     next bs = return trailersMaker ctx'
--   >       where
--   >         !ctx' = CH.hashUpdate ctx bs
--
--   Usage example:
--
--   > let h2rsp = responseFile ...
--   >     maker = trailersMaker (CH.hashInit :: Context SHA1)
--   >     h2rsp' = setResponseTrailersMaker h2rsp maker
data TrailersMaker = TrailersMaker [H.Header] (ByteString -> IO TrailersMaker)

-- | TrailersMake to create no trailers.
defaultTrailersMaker :: TrailersMaker
defaultTrailersMaker = TrailersMaker [] (\_ -> return defaultTrailersMaker)

----------------------------------------------------------------

-- | Running trailers-maker.
--
-- > bufferIO buf siz $ \bs -> tlrmkr (Just bs)
runTrailersMaker :: TrailersMaker -> Buffer -> Int -> IO TrailersMaker
runTrailersMaker (TrailersMaker _ f) buf siz = bufferIO buf siz $ \bs -> f bs
