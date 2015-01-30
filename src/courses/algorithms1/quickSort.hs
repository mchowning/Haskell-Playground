{-#OPTIONS_GHC -Wall -Werror #-}

import Test.HUnit
-- import Data.Tuple.Extra

quickSortTests :: Test
quickSortTests = TestList
  [ quickSort [] ~?= []
  , quickSort [1] ~?= [1]
  , quickSort [2,1] ~?= [1,2]
  , quickSort ([1,3..99] ++ [2,4..100]) ~?= [1..100] ]

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (p:xs) = quickSort lesser ++ [p] ++ quickSort greater
  where
    lesser  = filter (<p) xs
    greater = filter (>=p) xs

-- -- Just picking the first element in the list as the pivot
-- quickSort :: [Int] -> [Int]
-- quickSort []  = []
-- quickSort (pivot:ls) = let (front, back) = both quickSort . partition $ ls
--                        in front ++ [pivot] ++ back
--   where
--     -- This partition function requires n extra memory
--     partition :: [Int] -> ([Int], [Int])
--     partition = foldr partitionHelper ([],[])
--       where
--         partitionHelper :: Int -> ([Int], [Int]) -> ([Int], [Int])
--         partitionHelper n (front,back) | n < pivot = (n:front,back)
--                                    | otherwise = (front,n:back)

-- -- todo: attempt in place partition - apparently you need to use some black magic to do this in haskell
-- partition1 :: [Int] -> [Int]
-- partition1 ls = let = partition1Helper in
--   where
--     partition1Helper :: [Int] -> Int -> Int -> [Int]
--     partition1Helper (x:xs) pi ei | length xs == ei = let (front,back) = splitAt pi xs in front ++ [x] ++ back
--                                   | otherwise       = if xs !! pi < x then partitionHelper x:
