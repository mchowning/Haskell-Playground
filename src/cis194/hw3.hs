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
                , ("hello!", ["hello!", "el!", "l!", "l", "o", "!"])
                ]


--- Exercise 2

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

