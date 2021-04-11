{-# OPTIONS_GHC -Wall -Werror #-}

import Data.Char
import Test.HUnit


--- Exercise 1

toDigits :: Integer -> [Integer]
toDigits n | n <= 0    = []
           | otherwise = map (toInteger . digitToInt) . show $ n

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits
--toDigitsRev n | n <= 0    = []
--              | otherwise = (lastDigit n):(toDigitsRev . allButLastDigit $ n)
--    where
--        lastDigit :: Integer -> Integer
--        lastDigit a = a `mod` 10
--        allButLastDigit :: Integer -> Integer
--        allButLastDigit b = b `div` 10

runToDigitsTests :: IO Counts
runToDigitsTests = runTestTT allTests
  where
    allTests :: Test
    allTests = TestList $ map baseTest testCases
    baseTest :: (Integer, [Integer]) -> Test
    baseTest (input, result) = TestCase $ assertEqual "" result (toDigits input)
    testCases :: [(Integer, [Integer])]
    testCases = [ (123 , [1,2,3])
                , (1234, [1,2,3,4])
                , (4444, [4,4,4,4])]


--- Exercise 2

doubleEveryOtherFromRight :: [Integer] -> [Integer]
doubleEveryOtherFromRight = reverse . doubleEveryOtherFromLeft . reverse
  where
    doubleEveryOtherFromLeft = zipWith ($) (cycle [id,(*2)])

runDoubleEveryOtherFromRightTests :: IO Counts
runDoubleEveryOtherFromRightTests = runTestTT allTests
  where
    allTests :: Test
    allTests = TestList $ map baseTest testCases
    baseTest :: ([Integer], [Integer]) -> Test
    baseTest (input, result) = TestCase $ assertEqual "" result (doubleEveryOtherFromRight input)
    testCases :: [([Integer], [Integer])]
    testCases = [ ([1,2,3,4], [2,2,6,4])
                , ([1,1,1]  , [1,2,1])
                , ([1,1,1,1], [2,1,2,1])]



--- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits = sum . map sumDigit
  where
    sumDigit :: Integer -> Integer
    sumDigit = fromIntegral . sum . map digitToInt . show
--     sumDigit n | n < 10    = n
--                | otherwise = n `mod` 10 + sumDigit (n `div` 10)

runSumDigitsTests :: IO Counts
runSumDigitsTests = runTestTT allTests
  where
    allTests :: Test
    allTests = TestList $ map baseTest testCases
    baseTest :: ([Integer], Integer) -> Test
    baseTest (input, result) = TestCase $ assertEqual "" result (sumDigits input)
    testCases :: [([Integer], Integer)]
    testCases = [ ([1,1,1] , 3)
                , ([11,1]  , 3)
                , ([1,2,23], 8)]


--- Exercise 4

validate :: Integer -> Bool
validate = (==) 0 . flip mod 10 . sumDigits . doubleEveryOtherFromRight . toDigits

runValidateTests :: IO Counts
runValidateTests = runTestTT allTests
  where
    allTests :: Test
    allTests = TestList $ map baseTest testCases
    baseTest :: (Integer, Bool) -> Test
    baseTest (input, result) = TestCase $ assertEqual "" result (validate input)
    testCases :: [(Integer, Bool)]
    testCases = [ (4012888888881881, True)
                , (4012888888881882, False)]


--- Exercise 5 - Towers of Hanoi

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start goal other | n < 1     =  undefined
                         | n == 1    = [(start, goal)]
                         | otherwise = hanoi (n-1) start other goal ++ [(start, goal)] ++ hanoi (n-1) other goal start

runHanoiTests :: IO Counts
runHanoiTests = runTestTT allTests
  where
    allTests :: Test
    allTests = TestList $ hanoi15DiscTest : map baseTest testCases
    hanoi15DiscTest = TestCase $ assertEqual "" 32767 (length (hanoi 15 "a" "b" "c"))
    baseTest :: (Integer, Peg, Peg, Peg, [Move]) -> Test
    baseTest (i, p1, p2, p3, result) = TestCase $ assertEqual "" result (hanoi i p1 p2 p3)
    testCases :: [(Integer, Peg, Peg, Peg, [Move])]
    testCases = [ (3, "a", "b", "c", [("a","b"), ("a","c"), ("b","c"), ("a","b"), ("c","a"), ("c","b"), ("a","b")])
                , (1, "a", "b", "c", [("a","b")])
                , (2, "a", "b", "c", [("a","c"), ("a","b"), ("c","b")]) ]



--- Exercise 6 - Towers of Hanoi with 4 pegs

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n start goal other1 other2 | n < 3     = hanoi n start goal other1
                                  | otherwise = hanoi4 numMove start other1 goal other2 ++ hanoi numLeave start goal other2 ++ hanoi4 numMove other1 goal start other2
                                    where
                                      numLeave = div n 2
                                      numMove = n - numLeave

--FIXME failing test - I don't seem to be efficient enough!!!
runHanoi4Tests :: IO Counts
runHanoi4Tests = runTestTT $ TestCase $ assertEqual "" 129 (length (hanoi4 15 "a" "b" "c" "d"))

