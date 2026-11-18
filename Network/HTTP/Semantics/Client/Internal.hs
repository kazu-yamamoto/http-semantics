module Network.HTTP.Semantics.Client.Internal (
    Request (..),
    Response (..),
    Aux (..),
    defaultAux,

    -- * Giving up
    setRequestGiveUp,
    RequestGivenUp (..),
) where

import Control.Concurrent.STM (STM)
import Control.Exception (Exception)

import Network.HTTP.Semantics.Types (InpObj (..), OutObj (..))

-- | Request from client.
newtype Request = Request OutObj deriving (Show)

-- | Response from server.
newtype Response = Response InpObj deriving (Show)

-- | Additional information.
data Aux = Aux
    { auxPossibleClientStreams :: IO Int
    -- ^ How many streams can be created without blocking.
    , auxSendPing :: IO ()
    -- ^ Sending a ping.
    }

defaultAux :: Aux
defaultAux =
    Aux
        { auxPossibleClientStreams = return 0
        , auxSendPing = return ()
        }

-- | Giving up on the request once the STM action succeeds: the stream is
--   reset, and the wait for its response, or for its body, throws
--   'RequestGivenUp'.  'Network.HTTP.Semantics.Client.sendRequestWith'
--   is how a user gives up.
--
-- @since 0.5.0
setRequestGiveUp :: Request -> STM () -> Request
setRequestGiveUp (Request req) giveUp = Request req{outObjGiveUp = giveUp}

-- | What a library throws where it gave up on a request, for
--   'Network.HTTP.Semantics.Client.sendRequestWith' to turn into a
--   result.
--
-- @since 0.5.0
data RequestGivenUp = RequestGivenUp
    deriving (Show)

instance Exception RequestGivenUp
