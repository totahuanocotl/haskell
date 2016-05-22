{-# OPTIONS_GHC -Wall #-}
module LearningHaskell.HW07Tests
(
    fingerExercises
  , randomization
) where

import LearningHaskell.HW07

import Test.Tasty(testGroup, TestTree)
import Test.Tasty.HUnit
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V
import Prelude hiding (mapM)
import Control.Monad hiding (mapM, liftM)


fingerExercises :: TestTree
fingerExercises = testGroup "Finger Exercises"
    [
        testCase "liftM"           $ liftM (+1) (Just 5) @?= Just 6
      , testCase "swapV Just"      $ swapV 0 2 (V.fromList [1, 2, 3]) @?= Just (V.fromList [3, 2, 1])
      , testCase "swapV Nothing"   $ swapV 0 2 (V.fromList [1, 2]) @?= Nothing
      , testCase "mapM"            $ mapM Just [0..10] @?= Just [0..10]
    ]

randomization :: TestTree
randomization = testGroup "randomization"
    [
        testCase "getElts Just"    $ getElts [1,3] (V.fromList [0..9]) @?= Just [1, 3]
      , testCase "getElts Nothing" $ getElts [1,10] (V.fromList [0..9]) @?= Nothing
    ]

