{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module LearningHaskell.HW05Tests
(
    secret
  , victims
  , transactions
) where

import LearningHaskell.HW05
import LearningHaskell.Parser

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 as C8 (pack)
import Data.Map.Strict (Map)
import Data.Maybe
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

expectedSecretKey = "Haskell Is Great!"

resource :: String -> String
resource path = "../test/resources/HW05/" ++ path

secret :: TestTree
secret = testGroup "getSecret"
    [
        testCase "dog spy" $ do
            secret <- getSecret (resource "dog-original.jpg") (resource "dog.jpg")
            secret @?= expectedSecretKey
    ]

victims :: TestTree
victims = testGroup "victims"
    [
        testCase "victims" $ do
            let decryptedPath = resource "victims.json"
            decryptWithKey expectedSecretKey decryptedPath
            decryptedContent <- readFile decryptedPath
            expectedContent <- readFile (decryptedPath ++ ".expected")
            decryptedContent @?= expectedContent
    ]

transactions :: TestTree
transactions = testGroup "transactions"
    [
       testCase "parseFile non existing" $ do
            victims <- parseFile (resource "no_victims.json") :: IO (Maybe [TId])
            victims @?= Nothing
     , testCase "parseFile victims" $ do
                   victims <- parseFile (resource "victims.json") :: IO (Maybe [TId])
                   let contents (Just x) = x
                   length (contents victims) @?= 182
     , testCase "parseFile transactions" $ do
                   victims <- parseFile (resource "transactions.json") :: IO (Maybe [Transaction])
                   let contents (Just x) = x
                   length (contents victims) @?= 561
    ]