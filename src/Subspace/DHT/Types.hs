module Subspace.DHT.Types where

import Data.LargeWord (Word256)
import Data.Word (Word64)
import Network.Socket (SockAddr)
import Control.Monad.State (StateT)

-- Node

type NodeId = Word256

data Node = Node { nodeId :: NodeId
                 , nodeAddr :: SockAddr }

instance Show Node where
  show n = "(Node " ++ show (nodeId n) ++ ")"

-- State

type DHT a = StateT DHTState IO a

data DHTState = DHTState { nodes :: [Node] } deriving Show

-- Protocol

type PingId = Word64

data Query = Ping Node PingId
           | Pong Node PingId
           | GetNodes Node
           | SendNodes Node
