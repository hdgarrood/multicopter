module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Server.WebSocketsAppTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testCase "getGameId" test_getGameId
    ]
