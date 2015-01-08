{-#OPTIONS_GHC -Wall -Werror #-}

import Data.List
import Data.List.Split
import Test.HUnit

--- Exercise 1

skips :: [a] -> [[a]]
skips [] = [[]]
skips ls = reverse (foldl' (skipper ls) [] [1..(length ls)])
  where
    skipper :: [a] -> [[a]] -> Int -> [[a]]
    skipper xs acc n = (last . transpose . chunksOf n $ xs) : acc

runSkipsTests :: IO Counts
runSkipsTests = runTestTT allTests
  where
    allTests = TestList $ map baseTest testCases
    baseTest (input, expected) = expected ~=? skips input
    testCases = [ ([], [[]])
                , ("a", ["a"])
                , ("ABCD", ["ABCD", "BD", "C", "D"])
                , ("hello!", ["hello!", "el!", "l!", "l", "o", "!"]) ]


--- Exercise 2

-- todo switch to integers
localMaxima :: [Int] -> [Int]
localMaxima = foldr addIfMaxima [] . tails
 where
   addIfMaxima :: (Ord a) => [a] -> [a] -> [a]
   addIfMaxima (before:n:after:_) acc | n > before && n > after = n:acc
   addIfMaxima _                  acc = acc

runLocalMaximaTests :: IO Counts
runLocalMaximaTests = runTestTT allTests
  where
    allTests = TestList $ map baseTest testCases
    baseTest (input, expected) = expected ~=? localMaxima input
    testCases = [ ([]         , [])
                , ([2,9,5,6,1], [9,6])
                , ([2,3,4,1,5], [4])
                , ([1,2,3,4,5], []) ]


--- Exercise 3

-- todo - figure out how to make this shorter

histogram :: [Integer] -> String
histogram ils = unlines $ (plotHistogram . listCounter $ ils) ++ getBaseLines
  where
    -- ==========
    -- 0123456789
    getBaseLines :: [String]
    getBaseLines = [replicate 10 '=', ['0'..'9']]

    {- Takes a list with Integers between 0 and 9 and returns a 0 indexed 10-element list of
       integers that indicates the count of the numbers equal to each index -}
    listCounter :: [Integer] -> [Int]
    listCounter ls = foldr (counter ls) [] [0..9]
      where
        {- Appends to the accumulator the number of times the specified integer occurs in
           the specified list -}
        counter :: [Integer] -> Integer -> [Int] -> [Int]
        counter testLs i acc = (length . filter (== i) $ testLs) : acc

    plotHistogram :: [Int] -> [String]
    plotHistogram = reverse . transpose . convertToAsterisks
      where
        {- Takes a list of numbers and, in a list, for each number creates a string with the
           number of asterisks equal to the provided number, plus any spaces needed to make
           the String equal to the longest string returned by this function -}
        convertToAsterisks :: [Int] -> [String]
        convertToAsterisks tls = foldr (myReplicate (maximum tls)) [] tls

        {- Takes (fullStringSize) (numberOfAsterisks) (accumulatorList) and appends to the
           accumulator list a string that starts with the specified number of asterisks,
           plus any spaces needed to make the string length equal to the full string size -}
        myReplicate :: Int -> Int -> [String] -> [String]
        myReplicate limit n acc = (replicate (fromIntegral n) '*' ++ replicate (fromIntegral (limit - n)) ' ')  : acc


runHistogramTest :: IO Counts
runHistogramTest = runTestTT histogramTest
  where
    histogramTest :: Test
    histogramTest = TestList [ histogram h1Input ~?= h1Result
                             , histogram []      ~?= emptyResult ]

    h1Result = "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"

    emptyResult = "==========\n0123456789\n"

h1Input :: [Integer]
h1Input  = [1,4,5,4,6,6,3,4,2,4,9]
