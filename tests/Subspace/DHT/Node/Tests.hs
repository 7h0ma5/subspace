module Subspace.DHT.Node.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Subspace.DHT.Node
import Data.Binary
import Data.LargeWord
import Network.Socket

tests :: Test
tests = testGroup "Subspace.DHT.Node.Tests"
    [ testProperty "binary preservation" binaryPreservation
    , testProperty "zero distance" zeroDistance ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (LargeKey a b) where
  arbitrary = do
    i1 <- arbitrary
    i2 <- arbitrary
    return (LargeKey i1 i2)

instance Arbitrary Node where
  arbitrary = do
    nodeId <- arbitrary :: Gen Word256
    port <- arbitrary :: Gen Word16
    host <- arbitrary :: Gen Word32
    return (createNode nodeId (SockAddrInet (PortNum port) host))

binaryPreservation :: Node -> Bool
binaryPreservation origNode = origNode == newNode
   where newNode = decode . encode $ origNode

zeroDistance :: Node -> Bool
zeroDistance node = nodeDistance node node == 0
