module LearningHaskell.HW03.Test
(
   evalESuite
  ,extendSuite
  ,emptySuite
) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import LearningHaskell.HW03

emptySuite :: TestTree
emptySuite = testGroup "empty"
    [ testCase "non initialized" $ empty "x" @?= 0
    ]

extendSuite :: TestTree
extendSuite = testGroup "extend"
    [   testCase "state for x" $ initState "x" 9 "x" @?= 9
      , testCase "override x"  $ store "x" 3 (initState "x" 5) "x" @?= 3
      , testCaseSteps "state for x, y" $
            \step -> do
                let state = store "x" 3 (initState "y" 5)
                step "x is set"
                state "x" @?= 3
                step "y is set"
                state "y" @?= 5
    ]
    where initState = extend empty
          store key value state = extend state key value

evalESuite :: TestTree
evalESuite = testGroup "evalE"
    [ testCase "value"      $ evalE empty (Val 5) @?= 5
     ,testCase "variable"   $ evalE (extend empty "x" 9) (Var "x") @?= 9
     ,testCase "Plus"       $ apply 5 Plus 5 @?= 10
     ,testCase "Minus"      $ apply 5 Minus 5 @?= 0
     ,testCase "Times"      $ apply 5 Times 5 @?= 25
     ,testCase "Divide"     $ apply 5 Divide 5 @?= 1
     ,testCase "Gt - lt"    $ apply 5 Gt 6 @?= 0
     ,testCase "Gt - eq"    $ apply 5 Gt 5 @?= 0
     ,testCase "Gt - gt"    $ apply 6 Gt 5 @?= 1
     ,testCase "Ge - lt"    $ apply 5 Ge 6 @?= 0
     ,testCase "Ge - eq"    $ apply 5 Ge 5 @?= 1
     ,testCase "Ge - gt"    $ apply 6 Ge 5 @?= 1
     ,testCase "Lt - lt"    $ apply 5 Lt 6 @?= 1
     ,testCase "Lt - eq"    $ apply 5 Lt 5 @?= 0
     ,testCase "Lt - gt"    $ apply 6 Lt 5 @?= 0
     ,testCase "Le - lt"    $ apply 5 Le 6 @?= 1
     ,testCase "Le - eq"    $ apply 5 Le 5 @?= 1
     ,testCase "Le - gt"    $ apply 6 Le 5 @?= 0
     ,testCase "Eql - lt"   $ apply 5 Eql 6 @?= 0
     ,testCase "Eql - eq"   $ apply 5 Eql 5 @?= 1
     ,testCase "Eql - gt"   $ apply 6 Eql 5 @?= 0
    ]
    where apply a bop b = evalE empty (Op (Val a) bop (Val b))





