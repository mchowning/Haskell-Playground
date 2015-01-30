{-#OPTIONS_GHC -Wall -Werror #-}

import Test.HUnit

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort a = sortAsMerging (mergeSort firstHalf, mergeSort lastHalf)
  where
    (firstHalf, lastHalf) = splitAt (length a `div` 2) a

    sortAsMerging :: (Ord b) => ([b],[b]) -> [b]
    sortAsMerging ([], b) = b
    sortAsMerging (b, []) = b
    sortAsMerging (axs@(x:xs), ays@(y:ys)) | x < y     = x : sortAsMerging (xs,ays)
                                           | otherwise = y : sortAsMerging (axs, ys)

mergeSortTests :: Test
mergeSortTests =
  TestList [ mergeSort "" ~?= ""
           , mergeSort "a" ~?= "a"
           , mergeSort "ba" ~?= "ab"
           , mergeSort "fedcba" ~?= "abcdef"]

