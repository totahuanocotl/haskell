{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module LearningHaskell.HW05Tests
(
    secret
  , victims
  , parseJsonFile
  , badTransactions
  , moneyFlow
  , criminal
  , refunds
  , writeJsonFile
  , resource
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

moneyFlow :: TestTree
moneyFlow = testGroup "moneyFlow"
    [  testCase "getFlow" $ do
                let ts = [ Transaction { from = "Haskell Curry"
                           , to = "Simon Peyton Jones"
                           , amount = 10
                           , tid = "534a8de8-5a7e-4285-9801-8585734ed3dc"}
                         ]
                getFlow ts @?= Map.fromList [  ("Haskell Curry", -10)
                                             , ("Simon Peyton Jones", 10)]
    ]

criminal :: TestTree
criminal = testGroup "criminal"
    [  testCase "no Flow" $ do
                let flow =  Map.empty
                getCriminal flow @?= "No criminal"
     , testCase "getFlow" $ do
                let flow =  Map.fromList [  ("Isaac", -10)
                                           ,("Chris", 5)
                                           ,("James", 20)
                                           ]
                getCriminal flow @?= "James"
    ]

refunds :: TestTree
refunds = testGroup "refunds"
    [  testCase "no Flow" $ do
                let flow =  Map.empty
                undoTs flow ["1", "2"] @?= []

     , testCase "no ids" $ do
                let flow =  Map.fromList [  ("Isaac", -10)
                                           ,("James", 10)
                                           ]
                undoTs flow [] @?= []
     , testCase "simple flow" $ do
                let flow =  Map.fromList [  ("Isaac", -10)
                                           ,("James", 10)
                                           ]
                undoTs flow ["1"] @?= [ Transaction { from = "James"
                                                   , to = "Isaac"
                                                   , amount = 10
                                                   , tid = "1"}
                                       ]
     , testCase "multi flow" $ do
                let flow =  Map.fromList [  ("Isaac", -10)
                                          , ("Chris", -5)
                                          , ("James", 15)
                                         ]
                undoTs flow ["1", "2"] @?= [  Transaction { from = "James"
                                                   , to = "Isaac"
                                                   , amount = 10
                                                   , tid = "1"}
                                            , Transaction { from = "James"
                                                   , to = "Chris"
                                                   , amount = 5
                                                   , tid = "2"}
                                           ]
    ]

writeJsonFile :: TestTree
writeJsonFile = testGroup "writeJSON"
    [
       testCase "transactions" $ do
                let transactions = [  Transaction { from = "James"
                                          , to = "Chris"
                                          , amount = 5
                                          , tid = "1" }
                                    , Transaction { from = "James"
                                          , to = "Isaac"
                                          , amount = 5
                                          , tid = "2" }]
                writeJSON (resource "refunds.json") transactions
                refundsContent <- parseFile (resource "refunds.json") :: IO (Maybe [Transaction])
                pure length <*> refundsContent @?= Just 2
    ]
