{-# OPTIONS_GHC -Wall #-}
module LearningHaskell.HW06Tests
(
    fibonacciNaive
    , infiniteFibonacciNaive
    , infiniteFibonacci
    , streams
) where

import LearningHaskell.HW06

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Control.Applicative
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 as C8 (pack)
import Data.Map.Strict (Map)
import Data.Maybe
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

fibonacciNaive :: TestTree
fibonacciNaive = testGroup "fib"
    [
        testCase "fib 0" $ fib 0 @?= 0
      , testCase "fib 1" $ fib 1 @?= 1
      , testCase "fib 2" $ fib 2 @?= 1
      , testCase "fib 3" $ fib 3 @?= 2
      , testCase "fib 4" $ fib 6 @?= 8
      , testCase "fib 5" $ fib 15 @?= 610
--       , testCase "fib" $ fib 100 @?= 354224848179261915075
    ]

infiniteFibonacciNaive :: TestTree
infiniteFibonacciNaive = testGroup "infinite"
    [
        testCase "fibonacci naive" $ take 10 fibs1 @?= [1,1,2,3,5,8,13,21,34,55]
    ]

infiniteFibonacci :: TestTree
infiniteFibonacci = testGroup "infinite"
    [
        testCase "fibonacci" $ fibs2 !! 99 @?= 354224848179261915075
    ]

streams :: TestTree
streams = testGroup "stream"
    [
        testCase "repeat"     $ take 3 (streamToList ( sRepeat "x"))
                                @?= ["x","x","x"]
      , testCase "iterate"    $ take 3 (streamToList ( sIterate ('x' :) "o"))
                                @?= ["o", "xo", "xxo"]
      , testCase "interleave" $ take 3 (streamToList ( sInterleave (sRepeat 1) (sRepeat 2)))
                                @?= [1, 2, 1]
      , testCase "take"  $ sTake 3 (sRepeat "x") @?= ["x","x","x"]
      , testCase "nats"  $ sTake 5 nats          @?= [0,1,2,3,4]
      , testCase "ruler" $ sTake 64 ruler        @?= [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4
                                                     ,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,5
                                                     ,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4
                                                     ,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,6]
    ]

