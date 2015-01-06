{-#OPTIONS_GHC -Wall -Werror #-}

import Data.List
import Data.List.Split
import Test.HUnit

--- Exercise 1

-- TODO

skips :: [a] -> [[a]]
skips [] = [[]]
skips ls = map (skipper ls) [1..(length ls)]
  where
    skipper :: [a] -> Int -> [a]
    skipper xs n = case reverse . transpose . filter ((== n) . length) . chunksOf n $ xs of (x:_) -> x
                                                                                            _ -> []
--     helper xs n = last . transpose . filter ((== n) . length) . chunksOf n $ xs -- Bad form to use last???


runSkipsTests :: IO Counts
runSkipsTests = runTestTT allTests
  where
    allTests = TestList $ map baseTest testCases
    baseTest (input, expected) = expected ~=? skips input
    testCases = [ ("a", ["a"])
                , ("ABCD", ["ABCD", "BD", "C", "D"])
                , ("hello!", ["hello!", "el!", "l!", "l", "o", "!"])
                ]


--- Exercise 2

-- localMaxima :: [Integer] -> [Integer]
-- localMaxima = foldr getComparisons
--   where
--     checkMax ::
--
--
-- runLocalMaximaTests :: IO Counts
-- runLocalMaximaTests = runTestTT allTests
--   where
--     allTests = TestList $ map baseTest testCases
--     baseTest (input, expected) = expected ~=? localMaxima input
--     testCases = [ ([2,9,5,6,1], [9,6])
--                 , ([2,3,4,1,5], [4])
--                 , ([1,2,3,4,5], []) ]
