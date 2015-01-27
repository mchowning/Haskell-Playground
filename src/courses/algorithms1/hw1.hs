{-#OPTIONS_GHC -Wall -Werror #-}

import Test.HUnit

-- todo: try using Data.Text instead of String

-- result is 2407905288

inversionFinder :: IO ()
inversionFinder = do contents <- readFile "IntegerArray.txt"
                     print . fst . findInversions . convertToListOfIntegers $ contents
  where
    convertToListOfIntegers :: String -> [Int]
    convertToListOfIntegers = map read . lines

------------------------------------------------------------------------------------------------------------------------

findInversions :: (Ord a, Show a) => [a] -> (Int, [a])
findInversions []  = (0, [])
findInversions [a] = (0, [a])
findInversions ls  = let (leftHalf, rightHalf)        = splitAt (length ls `div` 2) ls
                         (nLeft, sortedLeft)          = findInversions leftHalf
                         (nRight, sortedRight)        = findInversions rightHalf
                         (nBetweenHalves, fullSorted) = countInversionsAndMerge 0 sortedLeft sortedRight
                         totalN                       = nLeft + nRight + nBetweenHalves
                     in (totalN, fullSorted)

findInversionsTest :: Test
findInversionsTest = TestList
  [ findInversions ""    ~?= (0, "")
  , findInversions "a"   ~?= (0, "a")
  , findInversions "ba"  ~?= (1, "ab")
  , findInversions "cba" ~?= (3, "abc")
  , findInversions "abc" ~?= (0, "abc") ]


------------------------------------------------------------------------------------------------------------------------

countInversionsAndMerge :: (Ord a, Show a) => Int -> [a] -> [a] -> (Int, [a])
countInversionsAndMerge n a [] = (n, a)
countInversionsAndMerge n [] a = (n, a)
countInversionsAndMerge n axs@(x:xs) ays@(y:ys)
  | x < y     = let (n1, sortedList) = countInversionsAndMerge n xs ays
                in (n1, x:sortedList)
  | otherwise = let (n1, sortedList) = countInversionsAndMerge (n + length axs) axs ys
                in (n1, y:sortedList)

countInversionsAndMergeTest :: Test
countInversionsAndMergeTest = TestList
  [ countInversionsAndMerge 0 "b" "a"   ~?= (1, "ab")
  , countInversionsAndMerge 1 "b" "a"   ~?= (2, "ab")
  , countInversionsAndMerge 0 "abc" "d" ~?= (0, "abcd")
  , countInversionsAndMerge 0 "d" "abc" ~?= (3, "abcd")]


