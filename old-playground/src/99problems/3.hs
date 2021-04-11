{-#OPTIONS_GHC -Wall -Werror #-} 

import Test.HUnit

--------------------------------------------------
-- Find the N'th element of a list.  The first elemen in the list is number 1.
--------------------------------------------------

-- standard haskell function usage
findElem1 :: [a] -> Int -> a
findElem1 ls i = ls !! (i - 1)


-- linear recursive process using tail
findElem2 :: [a] -> Int -> a
findElem2 ls i | i < 1 = undefined
               | i == 1 = head ls
               | otherwise = findElem2 (tail ls) (i - 1)

-- linear recursive process using pattern matching
findElem3 :: [a] -> Int -> a
findElem3 [] _ = undefined
findElem3 (x:_) 1 = x
findElem3 (_:xs) n 
    | n < 1 = undefined
    | otherwise = findElem3 xs (n - 1)


-- dropping all elements before the specified element and then taking the head of the list
findElem4 :: [a] -> Int -> a
findElem4 ls i | i < 1 = undefined
               | otherwise = head $ drop (i - 1) ls


-- taking the first N elements of a list and then taking the last element of that list
findElem5 :: [a] -> Int -> a
findElem5 ls n = last (take n ls)


--------------------------------------------------
-- Tests
--------------------------------------------------

-- To execute tests run "runTestTT tests" in the ghci terminal

handlesNumList :: ([Int] -> Int -> Int) -> Test
handlesNumList f = TestCase (assertEqual "handles Num lists"
    2
    (f [1, 2, 3] 2))

handlesString :: (String -> Int -> Char) -> Test
handlesString f = TestCase (assertEqual "handles Char lists"
    'e'
    (f "haskell" 5))

testableMethods :: [[a] -> Int -> a]
testableMethods = [findElem1, findElem2, findElem3, findElem4, findElem5]

tests :: Test
tests = TestList (map handlesNumList testableMethods ++
                  map handlesString testableMethods)
