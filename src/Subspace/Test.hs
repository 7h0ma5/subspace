module Main where

import Network.Socket
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Subspace.DHT.Connection
import Subspace.DHT.Network
import Subspace.DHT.Node
import Subspace.DHT.Protocol

main = do
  addr <- inet_addr "127.0.0.1"
  let first = Node 1337 (SockAddrInet 39495 addr)
  let conn = bootstrapConnection first
  runConnection 39495 conn
