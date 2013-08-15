module Subspace.DHT.Node where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.LargeWord
import Network.Socket
import Subspace.DHT.Types

createNode :: NodeId -> SockAddr -> Node
createNode id addr = Node { nodeId = id
                          , nodeAddr = addr }

nodeDistance :: Node -> Node -> Word256
nodeDistance a b = (nodeId a) `xor` (nodeId b)

instance Eq Node where
  (==) a b = (nodeId a) == (nodeId b)

instance Binary Node where
  put node = do
    putWord64be . hiHalf . hiHalf . hiHalf $ nodeId node
    putWord64be . loHalf . hiHalf . hiHalf $ nodeId node
    putWord64be . loHalf . hiHalf $ nodeId node
    putWord64be . loHalf $ nodeId node
    putWord32be addr
    putWord16be port
    where (SockAddrInet (PortNum port) addr) = nodeAddr node

  get = do
    w1 <- getWord64be
    w2 <- getWord64be
    w3 <- getWord64be
    w4 <- getWord64be
    let id = LargeKey w4 (LargeKey w3 (LargeKey w2 w1))
    addr <- getWord32be
    port <- getWord16be
    return (createNode id (SockAddrInet (PortNum port) addr))
