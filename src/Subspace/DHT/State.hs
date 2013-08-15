module Subspace.DHT.State
       ( bootstrapDHT
       , runDHT ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Data.Word (Word16)
import Network.Socket
import Subspace.DHT.Network
import Subspace.DHT.Node
import Subspace.DHT.Protocol
import Subspace.DHT.Types

initialDHT :: DHTState
initialDHT = DHTState { nodes = [] }

bootstrapDHT :: Node -> DHTState
bootstrapDHT firstNode = addNode firstNode initialDHT

addNode :: Node -> DHTState -> DHTState
addNode node state = state { nodes = node:(nodes state) }

handleQuery :: Query -> DHT ()

handleQuery (Ping node pingId) = do
  liftIO $ putStrLn "handle pint!"
  let n = Node 1338 (SockAddrInet 39495 0)
  modify $ addNode n
  state <- get
  return ()

handleQuery q = do
  liftIO $ putStrLn "handle query!"
  let n = Node 1338 (SockAddrInet 39495 0)
  modify $ addNode n
  state <- get
  return ()

runDHT :: Word16 -> DHTState -> IO ()
runDHT port state = do
  sock <- openPort (PortNum port)
  evalStateT (runDHT' sock) state

runDHT' :: SockHandle -> DHT ()
runDHT' sock = do
  mq <- liftIO $ do
    (pack, addr) <- recvPacket sock 4096
    return (decodeQuery pack)
  maybe (liftIO $ putStrLn "invalid query") (\query -> handleQuery query) mq
  state <- get
  liftIO $ print state
  runDHT' sock
