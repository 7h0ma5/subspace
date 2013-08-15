module Subspace.DHT.Protocol
       ( Query(..)
       , PingId
       , encodeQuery
       , decodeQuery
       ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy
import Data.LargeWord
import Data.Word
import Network.Socket
import Subspace.DHT.Node
import Subspace.DHT.Types

instance Show Query where
  show (Ping node id) = "(Ping " ++ show id ++ " from " ++ show node ++ ")"
  show (Pong node id) = "(Ping " ++ show id ++ " from " ++ show node ++ ")"
  show (GetNodes node) = "(GetNodes from " ++ show node ++ ")"
  show (SendNodes node) = "(SendNodes from " ++ show node ++ ")"

instance Binary Query where
  put (Ping node pingId) = do
    putWord8 0
    put node
    putWord64be pingId

  put (Pong node pingId) = do
    putWord8 1
    put node
    putWord64be pingId

  put (GetNodes node) = do
    putWord8 2

  put (SendNodes node) = do
    putWord8 3

  get = do
    qType <- getWord8
    case qType of
      0 -> do
        node <- get :: Get Node
        pingId <- get :: Get PingId
        return (Ping node pingId)
      1 -> do
        node <- get :: Get Node
        pingId <- get :: Get PingId
        return (Pong node pingId)
      2 -> do
        node <- get :: Get Node
        return (GetNodes node)
      3 -> do
        node <- get :: Get Node
        return (SendNodes node)
      _ ->
        fail "invalid query type"

encodeQuery :: Query -> ByteString
encodeQuery query = encode query

decodeQuery :: ByteString -> Maybe Query
decodeQuery bstr =
  case decodeOrFail bstr of
    Left _ -> Nothing
    Right (_, _, q) -> Just q
