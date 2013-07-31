module Subspace.DHT.Network
       ( SockHandle
       , openPort
       , closePort
       , recvPacket
       , sendPacket ) where

import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString (recvFrom, sendAllTo)
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS

data SockHandle = SockHandle { pSocket :: Socket, pNumber :: PortNumber }

-- | open a socket on the specified port
openPort :: PortNumber -> IO SockHandle
openPort port = do
  sock <- socket AF_INET Datagram 17
  setSocketOption sock Broadcast 1
  bindSocket sock (SockAddrInet port iNADDR_ANY)
  return $ SockHandle sock port

-- | close a socket
closePort :: SockHandle -> IO ()
closePort handle = sClose $ pSocket handle

-- | receive a packet with a specified maximum length from a socket
recvPacket :: SockHandle -> Int -> IO (LBS.ByteString, SockAddr)
recvPacket handle len = do
  (packet, addr) <- recvFrom (pSocket handle) len
  return (strictToLazyBS packet, addr)

-- | send a package to to an address via a socket
sendPacket :: SockHandle -> LBS.ByteString -> SockAddr -> IO ()
sendPacket handle str = sendAllTo (pSocket handle) (lazyToStrictBS str)

-- | convert a lazy ByteString to a strict ByteString
lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS x = BS.concat $ LBS.toChunks x

-- | convert a strict ByteString to a lazy ByteString
strictToLazyBS :: BS.ByteString -> LBS.ByteString
strictToLazyBS x = LBS.fromChunks [x]