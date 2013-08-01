module Subspace.DHT.Connection where

import Control.Exception
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Network.Socket
import Subspace.DHT.Network
import Subspace.DHT.Node
import Subspace.DHT.Protocol

data Connection = Connection { nodes :: [Node] }

initialConnection :: Connection
initialConnection = Connection []

bootstrapConnection :: Node -> Connection
bootstrapConnection firstNode = Connection [firstNode]

runConnection :: Word16 -> Connection -> IO ()
runConnection port conn = do
  sock <- openPort (PortNum port)
  forever $ do
    (pack, addr) <- recvPacket sock 512
    let q = decode pack :: Query
    return ()