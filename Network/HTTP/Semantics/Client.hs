{-# LANGUAGE RankNTypes #-}

module Network.HTTP.Semantics.Client where

import Network.HTTP.Semantics.Internal

----------------------------------------------------------------

-- | Send a request and receive its response.
type SendRequest = forall r. Request -> (Response -> IO r) -> IO r

-- | Client type.
type Client a = SendRequest -> Aux -> IO a

-- | Request from client.
newtype Request = Request OutObj deriving (Show)

-- | Response from server.
newtype Response = Response InpObj deriving (Show)

-- | Additional information.
data Aux = Aux
    { auxPossibleClientStreams :: IO Int
    -- ^ How many streams can be created without blocking.
    }
