module Subspace.DHT.Node where

import Network.Socket
import Data.LargeWord
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

type NodeId = Word256

data Node = Node { nodeId :: NodeId
                 , nodeAddr :: SockAddr }

instance Eq Node where
  (==) a b = (nodeId a) == (nodeId b)

instance Show Node where
  show n = "(Node " ++ (show $ nodeId n) ++ ")"

instance Binary Node where
  put (Node id (SockAddrInet (PortNum port) addr)) = do
    putWord64be . hiHalf . hiHalf . hiHalf $ id
    putWord64be . loHalf . hiHalf . hiHalf $ id
    putWord64be . loHalf . hiHalf $ id
    putWord64be . loHalf $ id
    putWord32be addr
    putWord16be port

  get = do
    w1 <- getWord64be
    w2 <- getWord64be
    w3 <- getWord64be
    w4 <- getWord64be
    let id = LargeKey w4 (LargeKey w3 (LargeKey w2 w1))
    addr <- getWord32be
    port <- getWord16be
    return (Node id (SockAddrInet (PortNum port) addr))