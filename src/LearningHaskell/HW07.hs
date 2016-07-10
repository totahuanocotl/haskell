{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module LearningHaskell.HW07 where

import Prelude hiding (mapM)
import LearningHaskell.Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= \x -> return $ f x

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV x y v = liftM2 swap (v !? x) (v !? y)
    where swap mY mX = v // [(x, mX), (y, mY)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f xs = sequence (map f xs)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts indices v = mapM (v !?) indices

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = getRandomR (0, V.length v) >>= \x -> return $ v !? x

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = replicateM n getRandom >>= \xs -> return $ V.fromList xs

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n range = replicateM n (getRandomR range) >>= \xs -> return $ V.fromList xs

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = shuffle' (V.length v - 1)
    where shuffle' 0 = return v
          shuffle' x = fmap swap (getRandomR (0, x))
            where swap y = v // [(x, v ! y), (y, v ! x)]


-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v n = (V.ifilter (value (< pivot)) v , pivot, V.ifilter (value (>= pivot)) v)
        where pivot = v ! n
              value f index element = index /= n && f element

partitionAt' :: Ord a => Vector a -> Int -> Maybe (Vector a, a, Vector a)
partitionAt' v n = v !? n >>= \pivot -> return (V.ifilter (value (< pivot)) v , pivot, V.ifilter (value (>= pivot)) v)
      where value f index element = index /= n && f element

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
    | V.null v = V.empty
    | otherwise = qsort' (< x) V.++ V.cons x (qsort' (>= x))
            where x  = V.head v
                  xs = V.tail v
                  qsort' f = qsort [y | y <- xs, f y ]
-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
    | V.null v = return V.empty
    | otherwise = getRandomR (0, V.length v - 1) >>= \pivot -> qsortR' (partitionAt v pivot)
                  where qsortR' (vl, e, vr) = do
                                              ql <- qsortR vl
                                              qr <- qsortR vr
                                              return $ ql V.++ V.cons e qr

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v
    | V.null v = return Nothing
    | otherwise = select' 0 v
                  where select' offset vs = getRandomR(0, V.length vs - 1) >>= \pivot ->  recurse (partitionAt vs pivot)
                            where recurse (vl, e, vr) = let lvl = V.length vl + offset in
                                                        if i == lvl then return $ Just e
                                                        else if i < lvl then select' offset vl
                                                        else select' (lvl + 1) vr


-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = undefined

newDeck :: Rnd Deck
newDeck =  undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

hw07main :: IO ()
hw07main = evalRandIO newDeck >>= repl . State 100
