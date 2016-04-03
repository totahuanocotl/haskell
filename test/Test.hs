module Test where

import Test.Tasty (defaultMain,testGroup,TestTree)

import LearningHaskell.HW03Tests
import LearningHaskell.HW04Tests


test :: IO ()
test = defaultMain testsW04

testsW03 :: TestTree
testsW03 = testGroup "Week 3 Tests"
            [   emptySuite
              , extendSuite
              , evalESuite
              , desugarSuite
              , evalSimpleSuite
              , runProgramSuite
            ]



testsW04 :: TestTree
testsW04 = testGroup "Week 4 Tests"
            [   polynomial
              , equality
              , display
              , addition
              , multiplication
              , negation
              , evaluation
              , differentiation
            ]