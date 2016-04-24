{-# OPTIONS_GHC -Wall #-}
module LearningHaskell.HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P l) == (P r) = significantTerms l  == significantTerms r

significantTerms :: (Num a, Eq a) => [a] -> [a]
significantTerms [0] = [0]
significantTerms xs  = reverse $ dropWhile (== 0) (reverse xs)

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = showSignificant $ map toTerm $ reverse $ filter ((/=0).fst) (zip p [0..])
                 where showSignificant terms
                        | null terms = "0"
                        | otherwise = intercalate " + " terms

toTerm :: (Num a, Eq a, Show a) => (a, Int) -> String
toTerm (c, 0) = show c
toTerm (1, 1) = "x"
toTerm (-1, 1) = "-x"
toTerm (c, 1) = show c ++ "x"
toTerm (c, e) = show c ++ "x^" ++ show e

-- Exercise 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P l) (P r) =  P (zipWith (+) (asDegree l) (asDegree r))
    where degree = max (length l) (length r)
          asDegree xs = xs ++ replicate (degree - length xs) 0


-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P ls) (P rs) =  sum $ map (\(c,e)-> shift e (mult c)) (zip ls [0..])
                       where mult n = P $ map (* n) rs
                             shift e (P xs) = P $ replicate e 0 ++ xs

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P terms) = P $ map negate terms
    fromInteger value = P [fromInteger value]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P terms) value = foldr (\(c, e) acc -> c * value ^ e + acc ) 0 (zip terms [0 :: Integer ..])

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 1 a = deriv a
    nderiv n a = nderiv (n-1) (deriv a)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P [_]) = P [0]
    deriv (P terms) =  P $ zipWith (*) (tail terms) (map fromInteger [1..])

