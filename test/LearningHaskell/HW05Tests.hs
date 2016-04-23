{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module LearningHaskell.HW05Tests
(
    secret
) where

import LearningHaskell.HW05
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 as C8 (pack)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

secret :: TestTree
secret = testGroup "getSecret"
    [
        testCase "dog spy" $ do
            secret <- getSecret "resources/HW05/dog-original.jpg" "resources/HW05/dog.jpg"
            secret @?= "Haskell Is Great!"
    ]