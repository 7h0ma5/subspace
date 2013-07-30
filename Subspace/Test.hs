module Main where

import Network.Socket
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Subspace.DHT.Network
import Subspace.DHT.Node
import Subspace.DHT.Protocol

main = do
    addr <- inet_addr "127.0.0.1"
    let n = Node 1337 (SockAddrInet 5004 addr)
    let r = Ping n 2342
    encodeFile "test.dat" r

    sock <- openPort 39495
    mainLoop sock

mainLoop :: SockHandle -> IO ()
mainLoop sock = do
    (pack, addr) <- recvPacket sock 512
    let query = decode pack :: Query
    print query
    mainLoop sock