-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------
testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, rev) = toRevDigits n == rev

testToDigits :: (Integer, [Integer]) -> Bool
testToDigits (n, dig) = toDigits n == dig

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
             [(1234, [4, 3, 2, 1]), (0, []), ((-17), [])]
           , Test "toDigits test" testToDigits
           [(1234, [1, 2, 3, 4]), (0, []), ((-17), [])]
    ]

-- Exercise 3 -----------------------------------------
testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (original, doubled) = doubleEveryOther original == doubled

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [([4, 9, 5, 5], [4, 18, 5, 10]) , ([0, 0], [0, 0]) , ([], []) , ([1], [1])]
    ]

-- Exercise 4 -----------------------------------------
testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (list, expected) = sumDigits list == expected

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits
             [([10, 5, 18, 4], 19)]
    ]

-- Exercise 5 -----------------------------------------
testLuhn :: (Integer, Bool) -> Bool
testLuhn (card, expected) = luhn card == expected

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
             [(5594589764218858, True), (1234567898765432, False)]
    ]

-- Exercise 6 -----------------------------------------
testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (discs, peg1, peg2, peg3, moves) = hanoi discs peg1 peg2 peg3 == moves

testHanoi4 :: (Integer, Peg, Peg, Peg, Peg, [Move]) -> Bool
testHanoi4 (discs, peg1, peg2, peg3, peg4, moves) = hanoi4 discs peg1 peg2 peg3 peg4 == moves

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi
             [ (1, "a", "b", "c", [("a", "c")])
              ,(2, "a", "b", "c", [("a","b"), ("a","c"), ("b","c")])
              ,(3, "a", "b", "c", [("a", "c"), ("a", "b"), ("c", "b"), ("a", "c"), ("b", "a"), ("b", "c"), ("a", "c")])]
           , Test "hanoi4 test" testHanoi4
             [ (1, "a", "b", "c", "d", [("a", "d")])
              ,(2, "a", "b", "c", "d", [("a", "b"),("a","d"),("b","d")])
              ,(3, "a", "b", "c", "d", [("a", "b"),("a","c"),("a", "d"),("c", "d"),("b", "d")])
              ,(4, "a", "b", "c", "d", [("a", "b"),("a","c"),("b", "c"),("a", "b"),("a", "d"),("b","d"),("c","a"),("c", "d"),("a","d")])]
    ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]