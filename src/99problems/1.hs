{-#OPTIONS_GHC -Wall -Werror #-} 

import Test.HUnit

--------------------------------------------------
-- Find the last element of a list
--------------------------------------------------


-- standard haskell function usage
myLast1 :: [a] -> a
myLast1 = last


myLast2 :: [a] -> a
myLast2 [] = undefined
myLast2 [x] = x
myLast2 (_:xs) = myLast2 xs


myLast3 :: [a] -> a
myLast3 xs =
    if length xs > 1
    then xs !! (length xs - 1)
    else undefined


myLast4 :: [a] -> a
myLast4 xs = 
    let listLength = length xs 
    in if listLength > 1
       then xs !! (listLength - 1)
       else undefined


myLast5 :: [a] -> a
myLast5 xs =
    if listLength > 0
    then xs !! (listLength - 1)
    else undefined
    where listLength = length xs


myLast6 :: [a] -> a
myLast6 = last


--------------------------------------------------
-- Tests
--------------------------------------------------

-- To execute tests run "runTestTT tests" in the ghci terminal

--handlesNumberLists :: (Show a, Eq a, Num a) => ([a] -> a) -> Test
handlesNumberLists :: ([Int] -> Int) -> Test
handlesNumberLists f = TestCase (assertEqual "handles number list,"
    4
    (f [1, 2, 3, 4]))

handlesCharLists :: (String -> Char) -> Test
handlesCharLists f = TestCase (assertEqual "handles Char list,"
    'z'
    (f "xyz"))


testableMethods :: [[a] -> a]
testableMethods = [myLast1, myLast2, myLast3, myLast4, myLast5, myLast6]

tests :: Test
tests = TestList (map handlesNumberLists testableMethods ++ 
                  map handlesCharLists testableMethods)
