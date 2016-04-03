module LearningHaskell.HW04Tests
(
    polynomial
  , equality
  , display
  , addition
  , multiplication
  , negation
  , evaluation
  , differentiation
) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import LearningHaskell.HW04

polynomial :: TestTree
polynomial = testGroup "x"
    [ testCase "f(x) = x" $ x @?= P [0, 1]
    ]

equality :: TestTree
equality = testGroup "equality"
    [   testCase "same"       $ P [1, 2, 3]                 @?= P [1, 2, 3]
      , testCase "equivalent" $ P [1, 2]                    @?= P [1, 2, 0]
      , testCase "different"  $ P [1, 2, 3] /= P [3, 2, 1]  @?= True
    ]

display :: TestTree
display = testGroup "show"
    [   testCase "polynomial Zero"  $ show (P [0, 0])       @?= "0"
      , testCase "coefficient"      $ show (P [-1])         @?= "-1"
      , testCase "neg. coefficient" $ show (P [1])          @?= "1"
      , testCase "x"                $ show (P [0, 1])       @?= "x"
      , testCase "-x"               $ show (P [0, -1])      @?= "-x"
      , testCase "quadratic"        $ show (P [0, 0, 2])    @?= "2x^2"
      , testCase "2x^3 + 1"         $ show (P [1, 0, 0, 2]) @?= "2x^3 + 1"
      , testCase "2x^2 + -x"        $ show (P [0, -1, 2])   @?= "2x^2 + -x"
    ]

addition :: TestTree
addition = testGroup "plus"
   [   testCase "coefficient"  $ plus (P [1]) (P [2])             @?= P [3]
     , testCase "linear"       $ plus (P [0, 1]) (P [1, 2])       @?= P [1, 3]
     , testCase "diff. degree" $ plus (P [1, 2]) (P [0, 0, 3, 4]) @?= P [1, 2, 3, 4]
   ]

multiplication :: TestTree
multiplication = testGroup "times"
   [   testCase "1 * 2"                    $ times (P [1]) (P [2])          @?= P [2]
     , testCase "2 * x"                    $ times (P [2]) (P [0, 1])       @?= P [0, 2]
     , testCase "2 * (1 + x)"              $ times (P [2]) (P [1, 1])       @?= P [2, 2]
     , testCase "x * x"                    $ times (P [0, 1]) (P [0, 1])    @?= P [0, 0, 1]
     , testCase "(1 + x) * 2"              $ times (P [2]) (P [1, 1])       @?= P [2, 2]
     , testCase "(1 + x) * (1 + x)"        $ times (P [1, 1]) (P [1, 1])    @?= P [1, 2, 1]
     , testCase "(1 + x) * (1 + 2x + x^2)" $ times (P [1, 1]) (P [1, 2, 1]) @?= P [1, 3, 3, 1]
   ]

negation :: TestTree
negation = testGroup "negate"
    [   testCase "1 + 2x + x^2" $ negate (P [1, 2, 1]) @?= P [ -1, -2, -1]
      , testCase "-1 -2x -x^2" $ negate (P [-1, -2, -1]) @?= P [ 1, 2, 1]
    ]

evaluation :: TestTree
evaluation = testGroup "applyP"
    [   testCase "x^2 + 2x + 1; x=1" $ applyP (P [1, 2, 1]) 1 @?= 4
      , testCase "x^2 + 2x + 1; x=2" $ applyP (P [1, 2, 1]) 2 @?= 9
    ]

differentiation :: TestTree
differentiation = testGroup "deriv"
    [   testCase "coefficient"   $ deriv (P [5])                @?= P [0]
      , testCase "2x + 3"        $ deriv (P [3, 2])             @?= P [2]
      , testCase "2x + 3"        $ deriv (P [3, 2])             @?= P [2]
      , testCase "(2x +3)^3 ; 3" $ nderiv 3 (P [27, 54, 36, 8]) @?= P [48]
    ]