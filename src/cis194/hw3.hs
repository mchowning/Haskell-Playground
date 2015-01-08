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

histogram :: [Integer] -> String
histogram = unlines . addBaseline . plotHistogram . countNumbers

  where

    {- Takes a list with Integers between 0 and 9 and returns a 0 indexed 10-element list of
       integers that indicates the count of the numbers equal to each index, i.e., if there
        were 2 zero's then the number 2 would be at the 0-index -}
    countNumbers :: [Integer] -> [Int]
    countNumbers ls = map (numberCounter ls) [0..9]
      where
      {- Determine how many times a given number occurs in a list -}
        numberCounter :: [Integer] -> Integer -> Int
        numberCounter ns n = length (filter (==n) ns)

    plotHistogram :: [Int] -> [String]
    plotHistogram = orientVertically . convertListToAsterisks
      where
        convertListToAsterisks :: [Int] -> [String]
        convertListToAsterisks ls = map (withSpaceIncreaseSizeTo (maximum ls) . createRowOfAsterisks) ls
          where
            createRowOfAsterisks :: Int -> String
            createRowOfAsterisks nAsterisks = replicate nAsterisks '*'

            withSpaceIncreaseSizeTo :: Int -> String -> String
            withSpaceIncreaseSizeTo l str = str ++ replicate (l - length str) ' '

        orientVertically :: [String] -> [String]
        orientVertically = reverse . transpose

    {- ==========
       0123456789 -}
    addBaseline :: [String] -> [String]
    addBaseline ls = ls ++ [replicate 10 '=', ['0'..'9']]


runHistogramTest :: IO Counts
runHistogramTest = runTestTT histogramTest
  where
    histogramTest :: Test
    histogramTest = TestList [ histogram h1Input ~?= h1Result
                             , histogram []      ~?= emptyResult ]
    h1Input  = [1,4,5,4,6,6,3,4,2,4,9]
    h1Result = "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"

    emptyResult = "==========\n0123456789\n"
