{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a = length . filter (uncurry (==)) . zip (a)

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (countColor) colors
                   where countColor color = length $ filter (== color) code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actual guess = sum $ map minimumOccurrence  $ zip (countColors actual) (countColors guess)
                       where minimumOccurrence pair = uncurry (min) pair

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = let guessed = exactMatches secret guess in
                       let almostGuessed = matches secret guess in
                       Move guess guessed (almostGuessed - guessed)

-- Exercise 4 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exact nonExact) code = let Move _ newExact newNonExact = getMove guess code in
                                                 newExact == exact && newNonExact == nonExact

-- Exercise 5 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
    | n <= 0    = []
    | n == 1    = map (\color -> [color]) colors
    | otherwise = concatMap generateCodes (allCodes (n - 1))
                  where generateCodes code = map (\color -> color : code) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = masterDumb secret (allCodes (length secret)) []
masterDumb :: Code -> [Code] -> [Move] -> [Move]
masterDumb _ [] moves = reverse moves
masterDumb secret (guess:codes) moves
          | guessed   = masterDumb secret [] (move : moves)
          | some      = masterDumb secret (filterCodes move codes) (move : moves)
          | otherwise = masterDumb secret codes (move : moves)
          where {
                    move    = getMove secret guess;
                    some    = matches secret guess > 0;
                    guessed = exactMatches secret guess == length secret;
                }

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = masterMind secret ([Red, Red, Blue, Blue] : (allCodes (length secret)) []
masterMind :: Code -> [Code] -> [Move] -> [Move]
masterMind secret (guess:codes) moves
          | guessed   = masterDumb secret [] (move : moves)
          | some      = masterDumb secret (filterCodes move codes) (move : moves)
          | otherwise = masterDumb secret codes (move : moves)
          where {
                    move    = getMove secret guess;
                    some    = matches secret guess > 0;
                    guessed = exactMatches secret guess == length secret;
                }
