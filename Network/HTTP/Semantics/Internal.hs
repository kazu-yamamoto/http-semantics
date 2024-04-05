module Network.HTTP.Semantics.Internal (
    -- * Types
    Scheme,
    Authority,
    Path,

    -- * Request and response
    InpObj (..),
    InpBody,
    OutObj (..),
    OutBody (..),
    FileSpec (..),
    FileOffset,
    ByteCount,
    TrailersMaker,
    NextTrailersMaker (..),
    defaultTrailersMaker,
) where

import Network.HTTP.Semantics.Types
