module Test where

import Test.Tasty (defaultMain,testGroup,TestTree)

import LearningHaskell.HW03Tests
import LearningHaskell.HW04Tests
import LearningHaskell.HW05Tests


test :: IO ()
test = defaultMain testsW05

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

testsW05 :: TestTree
testsW05 = testGroup "Week 5 Tests"
            [   secret
              , victims
              , parseJsonFile
              , badTransactions
              , moneyFlow
            ]