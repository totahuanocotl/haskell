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
-- resource path = "../test/resources/HW05/" ++ path
resource path = "test/resources/HW05/" ++ path

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
                let flow =  Map.fromList [  ("Aisha Craft",0)
                                            ,("Alessandra Blackmon",0)
                                            ,("Allen Delagarza",-668)
                                            ,("Altha Redden",-342)
                                            ,("Alvera Barrera",-462)
                                            ,("Alyson Pinkston",-437)
                                            ,("Andre Cummins",0)
                                            ,("Angelika Batson",0)
                                            ,("Angeline Keck",0)
                                            ,("Asha Walling",0)
                                            ,("August Jewell",-206)
                                            ,("Bettye Wooten",0)
                                            ,("Blondell See",0)
                                            ,("Brenna Woodall",0)
                                            ,("Candis Lemus",-220)
                                            ,("Caprice Burnett",-218)
                                            ,("Carline Epstein",0)
                                            ,("Charise Bradford",0)
                                            ,("Charlyn Keefer",0)
                                            ,("Cherie Mccool",0)
                                            ,("Colette Pickering",-173)
                                            ,("Daria Hawk",-102)
                                            ,("Dia Monson",-463)
                                            ,("Domenica Gable",-377)
                                            ,("Dona Link",-425)
                                            ,("Donald Sanford",0)
                                            ,("Doreatha Folse",-58)
                                            ,("Dorsey Gore",0)
                                            ,("Dulce Arnett",0)
                                            ,("Edris Corbitt",-398)
                                            ,("Elana Holden",-303)
                                            ,("Elise Carnes",-63)
                                            ,("Ellyn Furr",-287)
                                            ,("Ericka Beverly",0)
                                            ,("Farah Bateman",-197)
                                            ,("Faustina Wolford",0)
                                            ,("Felecia Mckeown",-204)
                                            ,("Felipa Sallee",-255)
                                            ,("Francie Duggan",-515)
                                            ,("Genaro Goins",0)
                                            ,("Gregory Nava",0)
                                            ,("Hayden Weatherford",-41)
                                            ,("Heike Champagne",-176)
                                            ,("Ignacia Baugh",-317)
                                            ,("Ileana Broughton",0)
                                            ,("JC Casas",-360)
                                            ,("Jami Kuntz",0)
                                            ,("Jason Mccallister",-63)
                                            ,("Jeanetta Hoyt",0)
                                            ,("Jennefer Eggleston",-339)
                                            ,("Jeraldine Williford",-384)
                                            ,("Jerrie Breedlove",0)
                                            ,("Joanie Cramer",0)
                                            ,("Johnson Humphrey",-620)
                                            ,("Jule Booker",0)
                                            ,("Kassie Hazel",0)
                                            ,("Khalilah Stout",-224)
                                            ,("Kia Burkholder",-362)
                                            ,("Leia Klinger",-291)
                                            ,("Lennie Haight",0)
                                            ,("Lissette Epperson",-206)
                                            ,("Louie Lovelace",0)
                                            ,("Lucie Yazzie",0)
                                            ,("Margeret Scully",-321)
                                            ,("Margurite Thorn",0)
                                            ,("Marlyn Pulido",0)
                                            ,("Matilda Albright",-124)
                                            ,("Maurice Comer",0)
                                            ,("Mollie Halverson",-236)
                                            ,("Nannie Swisher",0)
                                            ,("Nathanial Arce",-661)
                                            ,("Newton Burks",-228)
                                            ,("Ngan Bratcher",-205)
                                            ,("Ozella Burkett",0)
                                            ,("Reta Salcedo",-603)
                                            ,("Robby Laughlin",-151)
                                            ,("Roscoe Stamps",0)
                                            ,("Santo Sutherland",0)
                                            ,("Santos Maples",0)
                                            ,("Shaanan Cohney",13089)
                                            ,("Shanta Shafer",0)
                                            ,("Shanti Hanley",-107)
                                            ,("Sharice Bledsoe",0)
                                            ,("Shaunte Foy",0)
                                            ,("Suellen Boatwright",-261)
                                            ,("Teddy Cheng",0)
                                            ,("Teodoro Gold",0)
                                            ,("Torie Martino",0)
                                            ,("Una Nobles",-238)
                                            ,("Valarie Hoy",-198)
                                            ,("Virgen Herman",0)
                                           ]
                getCriminal flow @?= "Shaanan Cohney"
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
