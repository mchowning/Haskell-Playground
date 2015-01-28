{-#OPTIONS_GHC -Wall -Werror #-}

import Test.HUnit

{-
1. You are given as input an unsorted array of n distinct numbers, where n is a power of 2. Give an algorithm that
identifies the second-largest number in the array, and that uses at most n+log2n−2 comparisons.

2. You are a given a unimodal array of n distinct elements, meaning that its entries are in increasing order up until
its maximum element, after which its elements are in decreasing order. Give an algorithm to compute the maximum element
that runs in O(log n) time.

3. You are given a sorted (from smallest to largest) array A of n distinct integers which can be positive, negative, or
zero. You want to decide whether or not there is an index i such that A[i] = i. Design the fastest algorithm that you
can for solving this problem.

4. Give the best upper bound that you can on the solution to the following recurrence: T(1)=1 and T(n)≤T([sqrt(n)])+1 for
n>1. (Here [x] denotes the "floor" function, which rounds down to the nearest integer.)

5. You are given an n by n grid of distinct numbers. A number is a local minimum if it is smaller than all of its
neighbors. (A neighbor of a number is one immediately above, below, to the left, or the right. Most numbers have four
neighbors; numbers on the side have three; the four corners have two.) Use the divide-and-conquer algorithm design
paradigm to compute a local minimum with only O(n) comparisons between pairs of numbers. (Note: since there are n2
numbers in the input, you cannot afford to look at all of them. Hint: Think about what types of recurrences would give
you the desired upper bound.)
-}


--- 1



--- 2

unimodalMax :: [Int] -> Int
unimodalMax [a] = a
unimodalMax ls = let (leftHalf, rightHalf) = splitAt (length ls `div` 2) ls
                     goingUp = last leftHalf < head rightHalf
                 in unimodalMax (if goingUp then rightHalf else leftHalf)

unimodalMaxTests :: Test
unimodalMaxTests = TestList
  [ unimodalMax [1] ~?= 1
  , unimodalMax [1,2] ~?= 2
  , unimodalMax [2,1] ~?= 2
  , unimodalMax [1,2,3,4,5,6,5,4] ~?= 6
  , unimodalMax [4,3,2,1] ~?= 4
  , unimodalMax [3,4,3,2,1,0] ~?= 4
  , unimodalMax inputWithMaxOf650 ~?= 650 ]
    where
      inputWithMaxOf650 = [1..650] ++ [649,648..400]


--- 3

indexMatch :: [Int] -> Bool
indexMatch = indexMatchHelper 0
  where
    indexMatchHelper :: Int -> [Int] -> Bool
    indexMatchHelper _ [] = False
    indexMatchHelper startIndex [a] = startIndex == a
    indexMatchHelper startIndex ls  =
      case head rightHalf of
        x | x == rightHalfIndex -> True
          | x > rightHalfIndex  -> indexMatchHelper startIndex leftHalf
          | otherwise           -> indexMatchHelper rightHalfIndex rightHalf
      where
        (leftHalf, rightHalf) = splitAt (length ls `div` 2) ls
        rightHalfIndex = length leftHalf + startIndex

-- inefficient but simple, functional solution
-- indexMatch :: [Int] -> Bool
-- indexMatch ls = foldr (\(x,y) acc -> acc || x == y) False (zip [0..] ls)

indexMatchTests :: Test
indexMatchTests = TestList
  [ indexMatch [0] ~?= True
  , indexMatch [1] ~?= False
  , indexMatch [-1] ~?= False
  , indexMatch [-1,1] ~?= True
  , indexMatch [0,1] ~?= True
  , indexMatch [-1..6] ~?= False
  , indexMatch ([-1..5] ++ [7]) ~?= True
  , indexMatch ([-1,1] ++ [2..100]) ~?= True
  ]
