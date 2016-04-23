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
         keepFakes ids = filter (\t -> tid t `elem`  ids)

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty
getFlow (t:ts) = credit $ debit $ getFlow ts
    where
         credit = Map.insertWith (+) (to t) (amount t)
         debit  = Map.insertWith (+) (from t) (- (amount t))

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal flow = if Map.null flow then "No criminal" else criminal flow
    where criminal = fst . maximumBy (compare `on` fst) . Map.toList

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flow ids = let sortedByAmount = Data.List.sortBy (compare `on` fst) (Map.toList flow) in
              let payers = filter (\f -> snd f >= 0) sortedByAmount in
              let payees = filter (\f -> snd f < 0) sortedByAmount in
              refund payers payees ids

refund :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
refund [] _ _ = []
refund _ [] _ = []
refund _ _ [] = []
refund (payer:payers) (payee:payees) (txid:ids) =
                  tx : refund (debit payer ++ payers) (credit payee ++ payees) ids
                  where debit   = transfer negate
                        credit  = transfer abs
                        transfer op (name, balance) = if balance + op (amount tx) == 0 then [] else [(name, balance + op (amount tx))]
                        tx = Transaction { from = fst payer
                                                   , to = fst payee
                                                   , amount = min (abs (snd payee)) (snd payer)
                                                   , tid = txid }

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

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim