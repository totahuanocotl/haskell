{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module LearningHaskell.HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n
    | n <= 0 = 0
    | n <= 1 = 1
    | n == 2 = 1
    | otherwise = fib(n-2) + fib(n-1)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [1..]]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) y = Cons x (sInterleave y xs)

sTake :: Int -> Stream a -> [a]
sTake n _ | n <=0 = []
sTake n (Cons x xs) = x : sTake (n-1) xs


-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = Cons 0 (fmap (+1) nats)

ruler :: Stream Integer
-- I have no idea how i solved this 'cause i had lots of wine :) but it's awesome
ruler = sInterleave (sRepeat 0) (fmap (+1) ruler)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand n = let random = randomize (n-1) in
    Cons random (rand random)
    where randomize x = (1103515245 * x + 12345) `mod` 2147483648
-- Exercise 8 -----------------------------------------

{- Total Memory in use: 91 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax list = case list of
    []     -> Nothing
    (x:xs) -> Just $ parallelMinMax x x xs
        where parallelMinMax mn mx [] = (mn, mx)
              parallelMinMax !mn !mx (y:ys) = parallelMinMax (min mn y) (max mx y) ys

hw06main :: IO ()
hw06main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined