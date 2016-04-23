{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module LearningHaskell.HW05Tests
(
    secret
  , decrypt
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

expectedSecretKey = "Haskell Is Great!"

secret :: TestTree
secret = testGroup "getSecret"
    [
        testCase "dog spy" $ do
            secret <- getSecret (resource "dog-original.jpg") (resource "dog.jpg")
            secret @?= expectedSecretKey
    ]

decrypt :: TestTree
decrypt = testGroup "decrypt"
    [
        testCase "decrypt" $ do
            let decryptedPath = resource "victims.json"

            decryptWithKey expectedSecretKey decryptedPath

            decryptedContent <- readFile decryptedPath
            expectedContent <- readFile (decryptedPath ++ ".expected")
            decryptedContent @?= expectedContent
    ]

resource :: String -> String
resource path = "../test/resources/HW05/" ++ path