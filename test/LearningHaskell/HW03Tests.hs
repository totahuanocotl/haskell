module LearningHaskell.HW03Tests
(
    emptySuite
  , extendSuite
  , evalESuite
  , desugarSuite
  , evalSimpleSuite
  , runProgramSuite
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

desugarSuite :: TestTree
desugarSuite = testGroup "desugar"
    [   testCase "Skip"     $ desugar Skip @?= DSkip
      , testCase "Assign"   $ desugar (Assign "x" x) @?= DAssign "x" x
      , testCase "If"       $ desugar (If x Skip Skip) @?= DIf x DSkip DSkip
      , testCase "While"    $ desugar (While x Skip) @?= DWhile x DSkip
      , testCase "Sequence" $ desugar (Sequence Skip Skip) @?= DSequence DSkip DSkip
      , testCase "Incr"     $ desugar (Incr "x") @?= DAssign "x" (Op (Var "x") Plus (Val 1))
      , testCase "For"      $ desugar (For (Assign "x" (Val 0)) (Op x Le y) (Incr "x") Skip) @?=
                                       DSequence
                                        (DAssign "x" (Val 0))
                                        (DWhile (Op x Le y)
                                            (DSequence
                                                DSkip
                                                (desugar (Incr "x"))))
    ]
    where x = Var "x"
          y = Var "y"

evalSimpleSuite :: TestTree
evalSimpleSuite = testGroup "evalSimple"
    [
        testCase "Skip" $ eval DSkip "A" @?= 0
      , testCase "Assign" $ eval (DAssign "A" (Val 10)) "A" @?= 10
      , testCase "If - then" $ eval (DIf true (assign "A" 2) (assign "A" 4)) "A" @?= 2
      , testCase "If - else" $ eval (DIf false (assign "A" 2) (assign "A" 4)) "A" @?= 4
      , testCase "While" $ eval (DWhile (Op (Var "A") Le (Val 3)) (desugar (Incr "A"))) "A" @?= 4
      , testCaseSteps "Sequence" $
            \step -> do
                let state = evalSimple (eval (assign "x" 1)) (assign "y" 2)
                step "stmt1"
                state "x" @?= 1
                step "stmt2"
                state "y" @?= 2
    ]
    where eval = evalSimple empty
          true = Val 1
          false = Val 0
          assign x value = DAssign x (Val value)

runProgramSuite :: TestTree
runProgramSuite = testGroup "run"
    [
        testCase "Factorial 4" $ run (extend empty "In" 4) factorial "Out" @?= 24
      , testCase "Square root 123" $ run (extend empty "A" 123) squareRoot "B" @?= 11
      , testCase "Fibonacci 10" $ run (extend empty "In" 10) fibonacci "Out" @?= 89
    ]