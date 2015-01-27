{-#OPTIONS_GHC -Wall -Werror #-}

import Test.HUnit

-- result is 2407905288

inversionFinder :: IO ()
inversionFinder = do contents <- readFile "IntegerArray.txt"
                     print . countInversions . convertToListOfIntegers $ contents
  where
    convertToListOfIntegers :: String -> [Int]
    convertToListOfIntegers = map read . lines

------------------------------------------------------------------------------------------------------------------------

countInversions :: (Show a, Ord a) => [a] -> Int
countInversions = fst . findInversions

countInversionsTest :: Test
countInversionsTest = TestList
  [ countInversions ""    ~?= 0
  , countInversions "a"   ~?= 0
  , countInversions "ba"  ~?= 1
  , countInversions "ab"  ~?= 0
  , countInversions "cba" ~?= 3
  , countInversions "acb" ~?= 1
  , countInversions input ~?= 2372 ]
    where
      input=[4,80,70,23,9,60,68,27,66,78,12,40,52,53,44,8,49,28,18,46,21,39,51,7,87,99,69,62,84,6,79,67,14,98,83,0,96,5,82,10
            ,26,48,3,2,15,92,11,55,63,97,43,45,81,42,95,20,25,74,24,72,91,35,86,19,75,58,71,47,76,59,64,93,17,50,56,94,90,89
            ,32,37,34,65,1,73,41,36,57,77,30,22,13,29,38,16,88,61,31,85,33,54] :: [Int]

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


