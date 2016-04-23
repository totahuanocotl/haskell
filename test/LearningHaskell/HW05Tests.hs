module LearningHaskell.HW05Tests
(
    secret
) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import LearningHaskell.HW04

secret :: TestTree
secret = testGroup "getSecret"
    [
    ]