module Subspace.DHT.State
       ( initDHT
       , bootstrapDHT
       , runDHT ) where

import Control.Monad.State
import Network.Socket
import Subspace.DHT.Network
import Subspace.DHT.Node
import Subspace.DHT.Protocol

type DHT a = StateT DHTState IO a

data DHTState = DHTState { closeNodes :: [Node]
                         , friendNodes :: [Node]
                         , sockHandle :: SockHandle } deriving Show

initDHT :: SockHandle -> DHTState
initDHT handle = DHTState { closeNodes = []
                          , friendNodes = []
                          , sockHandle = handle }

bootstrapDHT :: Node -> DHTState -> DHTState
bootstrapDHT = addCloseNode

addCloseNode :: Node -> DHTState -> DHTState
addCloseNode node dht = dht { closeNodes = node:(closeNodes dht) }

sendQuery :: Query -> SockAddr -> DHT ()
sendQuery query addr = do
  let bin = encodeQuery query
  handle <- gets sockHandle
  liftIO $ sendPacket handle bin addr

handleQuery :: Query -> SockAddr -> DHT ()

handleQuery (Ping node pingId) addr = do
  liftIO $ putStrLn "handle ping!"
  sendQuery (Pong node pingId) addr
  return ()

handleQuery q addr = do
  liftIO $ putStrLn "handle query!"
  let n = Node 1338 (SockAddrInet 39495 0)
--  modify $ addNode n
  return ()

runDHT :: DHTState -> IO ()
runDHT state = print state >> evalStateT runDHT' state

runDHT' :: DHT ()
runDHT' = do
  sock <- gets sockHandle
  (mq, addr) <- liftIO $ do
    (pack, addr) <- recvPacket sock 4096
    return ((decodeQuery pack), addr)
  maybe (liftIO $ putStrLn "invalid query") (\query -> handleQuery query addr) mq
  runDHT'
