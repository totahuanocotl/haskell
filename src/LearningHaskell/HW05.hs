{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module LearningHaskell.HW05 where

import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Bits (xor)
import Data.List
import Data.Function (on)
import Control.Applicative
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import LearningHaskell.Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret left right = do
    leftContent <- BS.readFile left
    rightContent <- BS.readFile right
    return $ BS.pack $ filter (/=0) (BS.zipWith xor leftContent rightContent)

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key destination = do
    let encryptedFile = destination ++ ".enc"
    encrypted <- BS.readFile encryptedFile
    BS.writeFile destination (BS.pack $ BS.zipWith xor encrypted (decryptKey key))
    where
        decryptKey k = BS.concat [k | _ <- [0 :: Integer ..]]

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
    result <- try(BS.readFile path) :: IO (Either SomeException ByteString)
    case result of
        Left _ -> return Nothing
        Right contents -> return $ decode contents

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victims transactions= do
    fakeTransactionIds <- parseFile victims :: IO (Maybe[TId])
    allTransactions <- parseFile transactions :: IO (Maybe[Transaction])
    return $ pure keepFakes <*> fakeTransactionIds <*> allTransactions
    where
          keepFakes ids = filter ((`elem` ids) . tid )

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty
getFlow (t:ts) = credit $ debit $ getFlow ts
    where
          credit = Map.insertWith (+) (to t)   (amount t)
          debit  = Map.insertWith (+) (from t) (- (amount t))

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal flow = if Map.null flow then "No criminal" else criminal flow
    where
          criminal = fst . maximumBy (compare `on` snd) . Map.toList

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flow = refund (desc payers) (asc payees)
    where
          people = Map.toList flow
          payers = filter ((0<) . snd) people
          payees = filter ((0>) . snd) people
          asc    = sortBy (compare `on` snd)
          desc   = sortBy (flip compare `on` snd)

refund :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
refund [] _ _ = []
refund _ [] _ = []
refund _ _ [] = []
refund (payer:payers) (payee:payees) (txId:ids) =
    tx : refund (payers `afterBalance` debit payer) (payees `afterBalance` credit payee) ids
    where
          transfer = min (abs(snd payee)) (snd payer)
          balance (_, current)   = current
          debit (name, current)  = (name, current - transfer)
          credit (name, current) = (name, current + transfer)
          tx = Transaction {   from = fst payer
                             , to = fst payee
                             , amount = transfer
                             , tid = txId }
          afterBalance people person = if balance person == 0
                                       then people
                                       else person : people

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path = BS.writeFile path . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

hw05main :: IO ()
hw05main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "test/resources/HW05/dog-original.jpg"
                        "test/resources/HW05/dog.jpg"
                        "test/resources/HW05/transactions.json"
                        "test/resources/HW05/victims.json"
                        "test/resources/HW05/new-ids.json"
                        "test/resources/HW05/new-transactions.json"
  putStrLn crim