module Network.HTTP.Semantics.Header where

import Data.Array (Array)

import Network.HTTP.Semantics.Token

import Data.ByteString (ByteString)

-- | Header name.
type HeaderName = ByteString

-- | Header value.
type HeaderValue = ByteString

-- | Header.
type Header = (HeaderName, HeaderValue)

-- | Header list.
type HeaderList = [Header]

-- | An array to get 'HeaderValue' quickly.
--   'getHeaderValue' should be used.
--   Internally, the key is 'tokenIx'.
type ValueTable = Array Int (Maybe HeaderValue)

-- | A pair of token list and value table.
type HeaderTable = (TokenHeaderList, ValueTable)

-- | TokenBased header.
type TokenHeader = (Token, HeaderValue)

-- | TokenBased header list.
type TokenHeaderList = [TokenHeader]
