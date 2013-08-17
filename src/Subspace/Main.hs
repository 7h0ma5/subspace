module Main where

import Network.Socket
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Subspace.DHT.State
import Subspace.DHT.Network
import Subspace.DHT.Node
import Subspace.DHT.Protocol

main = do
  addr <- inet_addr "127.0.0.1"
  let first = Node 1337 (SockAddrInet 39495 addr)
  let test = Ping first 1234
  encodeFile "test.dat" test
  sockHandle <- openPort (PortNum 39495)
  let dht = bootstrapDHT first $ initDHT sockHandle
  runDHT dht
  return ()
