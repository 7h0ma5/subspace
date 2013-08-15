module Main where

import Network.Socket
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Subspace.DHT.State
import Subspace.DHT.Network
import Subspace.DHT.Node
import Subspace.DHT.Protocol
import Subspace.DHT.Types

main = do
  addr <- inet_addr "127.0.0.1"
  let first = Node 1337 (SockAddrInet 39495 addr)
  let test = Ping first 1234
  encodeFile "test.dat" test
  let conn = bootstrapDHT first
  runDHT 39495 conn
