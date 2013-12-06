module Main where

import Test.Framework
--import Test.Framework.Providers.QuickCheck2
--import Test.Framework.Providers.HUnit

import TestUtils
import Server.WebSocketsAppTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testWithInputs "getGameId" test_getGameId data_getGameId
    ]
