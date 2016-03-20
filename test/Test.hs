module Test where

import Test.Tasty (defaultMain,testGroup,TestTree)

import LearningHaskell.HW03.Test

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [   emptySuite
              , extendSuite
              , evalESuite
            ]
