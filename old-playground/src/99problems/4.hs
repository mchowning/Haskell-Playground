{-#OPTIONS_GHC -Wall -Werror #-} 

import Test.HUnit

--------------------------------------------------
-- Find the number of elements in a list
--------------------------------------------------


-- standard haskell function usage
myLength1 :: [a] -> Int
myLength1 = length


-- linear recursive process using init
myLength2 :: [a] -> Int
myLength2 [] = 0
myLength2 ls = 1 + myLength2 (init ls)

--linear recursive process using pattern matching
myLength3 :: [a] -> Int
myLength3 [] = 0
myLength3 (_:xs) = 1 + myLength3 xs


-- linear iterative process using pattern matching
myLength4 :: [a] -> Int
myLength4 ls = myLength4' ls 0
    where
        myLength4' :: [a] -> Int -> Int
        myLength4' [] n = n
        myLength4' (_:xs) n = myLength4' xs (n + 1)


-- use an infinite list to count the elements
myLength5 :: [a] -> Int
myLength5 [] = 0
myLength5 xs = fst $ last $ zip [1..] xs


-- map all elements to 1 and add
myLength6 :: [a] -> Int
myLength6 = sum . map (\_->1) --Hask


--------------------------------------------------
-- Tests
--------------------------------------------------

-- To execute tests run "runTestTT tests" in the ghci terminal

handlesNumList :: ([Int] -> Int) -> Test
handlesNumList f = TestCase (assertEqual "handles Num list,"
    3
    (f [123, 456, 789]))

handlesString :: (String -> Int) -> Test
handlesString f = TestCase (assertEqual "handles Strings,"
    13
    (f "Hello, world!"))

handlesEmptyList :: ([a] -> Int) -> Test
handlesEmptyList f = TestCase (assertEqual "handles an empty list,"
    0
    (f []))

testableMethods :: [[a] -> Int]
testableMethods = [myLength1, myLength2, myLength3, myLength4, myLength5, myLength6]

tests :: Test
tests = TestList (map handlesNumList testableMethods ++
                  map handlesString testableMethods ++
                  map handlesEmptyList testableMethods)


