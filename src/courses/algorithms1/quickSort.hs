{-#OPTIONS_GHC -Wall -Werror #-}

import Test.HUnit
import Data.Tuple.Extra

quickSortTests :: Test
quickSortTests = TestList
  [ quickSort [] ~?= []
  , quickSort [1] ~?= [1]
  , quickSort [2,1] ~?= [1,2]
  , quickSort ([1,3..99] ++ [2,4..100]) ~?= [1..100] ]

-- Just picking the first element in the list as the pivot
quickSort :: [Int] -> [Int]
quickSort []  = []
quickSort [a] = [a]
quickSort (pivot:ls) = let (front, back) = both quickSort . partition $ ls
                       in front ++ [pivot] ++ back
  where
    -- This partition function requires n extra memory
    partition :: [Int] -> ([Int], [Int])
    partition = foldr partitionHelper ([],[])
      where
        partitionHelper :: Int -> ([Int], [Int]) -> ([Int], [Int])
        partitionHelper n (front,back) | n < pivot = (n:front,back)
                                   | otherwise = (front,n:back)
