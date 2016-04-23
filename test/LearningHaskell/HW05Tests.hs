{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module LearningHaskell.HW05Tests
(
    secret
  , victims
  , parseJsonFile
  , badTransactions
) where

import LearningHaskell.HW05
import LearningHaskell.Parser

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Control.Applicative
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 as C8 (pack)
import Data.Map.Strict (Map)
import Data.Maybe
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

expectedSecretKey :: ByteString
expectedSecretKey = "Haskell Is Great!"

resource :: String -> String
resource path = "../test/resources/HW05/" ++ path

secret :: TestTree
secret = testGroup "getSecret"
    [
        testCase "dog spy" $ do
                 secretKey <- getSecret (resource "dog-original.jpg") (resource "dog.jpg")
                 secretKey @?= expectedSecretKey
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

parseJsonFile :: TestTree
parseJsonFile = testGroup "parseFile"
    [
       testCase "non existing" $ do
                noVictims <- parseFile (resource "no_victims.json") :: IO (Maybe [TId])
                pure length <*> noVictims @?= Nothing
     , testCase "victims" $ do
                victimTransactions <- parseFile (resource "victims.json") :: IO (Maybe [TId])
                pure length <*> victimTransactions @?= Just 182
     , testCase "transactions" $ do
                allTransactions <- parseFile (resource "transactions.json") :: IO (Maybe [Transaction])
                pure length <*> allTransactions @?= Just 561
    ]

badTransactions :: TestTree
badTransactions = testGroup "Transactions"
    [
       testCase "bad Transactions" $ do
                transactions <- getBadTs (resource "victims.json.expected") (resource "transactions.json") :: IO (Maybe [Transaction])
                pure length <*> transactions @?= Just 182
    ]