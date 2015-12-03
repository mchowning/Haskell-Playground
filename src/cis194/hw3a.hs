-- {-#OPTIONS_GHC -Wall -Werror #-}

import Data.List
import Test.HUnit
import Data.Char

--- Exercise 1

skips :: [a] -> [[a]]
skips ls =
  map (everyNOther ls) [1..(length ls)]
  where
    everyNOther :: [a] -> Int -> [a]
    everyNOther ls' n = map snd (filter (isMultiple n . fst) (zip [1..] ls'))

    isMultiple :: Int -> Int -> Bool
    isMultiple n = (== 0) . (`mod` n)

    -- TODO why can't I define the type if I just use the original ls variable here instead of passing it?
--     everyNOther :: Int -> [a]
--     everyNOther n = map snd (filter ((isMultiple n) . fst) (zip [1..] ls))


runSkipsTests :: IO Counts
runSkipsTests = runTestTT allTests
  where
    allTests = TestList
      [ ["ABCD", "BD", "C", "D"]               ~=? skips "ABCD"
      , ["hello!", "el!", "l!", "l", "o", "!"] ~=? skips "hello!"
      , ["a"]                                  ~=? skips "a"
      , ([]::[[Bool]])                         ~=? skips ([]::[Bool])
      -- TODO what is going on here?
--       , ([[]]::[[Bool]])                       ~=? skips ([]::[Bool])
--       , [""]                                   ~=? skips ""
      ]


--- Exercise 2


localMaxima :: [Integer] -> [Integer]
localMaxima ls =
  let triplets = map (take 3) (filter ((>2) . length) (tails ls)) in
  map (!! 1) (filter midIsMax triplets)
  where
    midIsMax :: Ord a => [a] -> Bool
    midIsMax [before,mid,after] = mid > maximum [before,after]
    midIsMax _                  = undefined

-- localMaxima (x:xs@(y:z:_)) | y > maximum [x,z] = y : localMaxima xs
--                            | otherwise         = localMaxima xs
-- localMaxima _ = []

runLocalMaximaTests :: IO Counts
runLocalMaximaTests = runTestTT allTests
  where
    allTests = TestList $ map baseTest testCases
    baseTest (input, expected) = expected ~=? localMaxima input
    testCases = [ ([]             , [])
                , ([2,9,5,6,1,2,1], [9,6,2])
                , ([2,9,5,6,1]    , [9,6])
                , ([2,3,4,1,5]    , [4])
                , ([1,2,3,4,5]    , []) ]


--- Exercise 3

histogram :: [Int] -> String
histogram ls = unlines . reverse . transpose . generateHistogramLines . generateCountData $ ls

generateCountData :: [Int] -> [Int]
generateCountData ls = map numMatches [0..9]
  where
    numMatches :: Int -> Int
    numMatches x = length . filter (== x) $ ls

generateHistogramLines :: [Int] -> [String]
generateHistogramLines ls = let max = maximum ls
                                tuple = zip [0..] ls
                            in map (generateLine max) (zip [0..] ls)

generateLine :: Int -> (Int, Int) -> String
generateLine max (index, count) = intToDigit index : '=' : replicate count '*' ++ replicate (max-count) ' '


runHistogramTest :: IO Counts
runHistogramTest = runTestTT histogramTest
  where
    histogramTest :: Test
    histogramTest = TestList [ histogram h1Input ~?= h1Result
                             , histogram []      ~?= emptyResult ]
    h1Input  = [1,4,5,4,6,6,3,4,2,4,9]
    h1Result = "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"

    emptyResult = "==========\n0123456789\n"
