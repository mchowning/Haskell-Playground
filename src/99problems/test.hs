--{-#OPTIONS_GHC -Wall -Werror #-} 

import Control.Applicative
import Test.HUnit

--------------------------------------------------
-- Find the last element of a list
--------------------------------------------------

myLast :: [a] -> a
myLast = last

myLast' :: [a] -> a
myLast' [] = undefined
myLast' [a] = a
myLast' (_:xs) = myLast xs

myLast'' :: [a] -> a
myLast'' xs =
    if length xs > 1
    then xs !! (length xs - 1)
    else undefined

myLast''' :: [a] -> a
myLast''' xs = 
    let listLength = length xs 
    in if listLength > 1
       then xs !! (listLength - 1)
       else undefined

myLast'''' :: [a] -> a
myLast'''' xs =
    if listLength > 1
    then xs !! (listLength - 1)
    else undefined
    where listLength = length xs


--------------------------------------------------
-- Tests
--------------------------------------------------

-- To execute tests run "runTestTT tests" in the ghci terminal

handlesNumberLists :: (Show a, Eq a, Num a) => ([a] -> a) -> Test
--handlesNumberLists :: ([Int] -> Int) -> Test
handlesNumberLists f = TestCase (assertEqual "handles number list,"
    (f [1, 2, 3, 4])
    4)

-- handlesNumberLists :: ([Int] -> Int) -> Test
-- handlesNumberLists' f = TestCase (assertEqual "handles number list,"
--     (f [1, 2, 3, 4])
--     4)

--handlesCharLists :: (Show a, Eq a) => ([a] -> a) -> Test
handlesCharLists :: ([Char] -> Char) -> Test
handlesCharLists f = TestCase (assertEqual "handles Char list,"
    (f ['x', 'y', 'z'])
    'z')


testableMethods :: [([a] -> a)]
testableMethods = [myLast, myLast', myLast'', myLast''', myLast'''']

testingMethods :: [([Int] -> Int) -> Test]
testingMethods = [handlesNumberLists]
-- testingMethods = [handlesNumberLists, handlesNumberLists']
-- testingMethods :: (Show a, Eq a) => [([a] -> a) -> Test]
-- testingMethods = [handlesNumberLists, handlesCharLists]

tests :: Test
tests = TestList (map handlesNumberLists testableMethods ++ 
                  map handlesCharLists testableMethods)
-- tests = TestList (testingMethods <*> testableMethods)



-- handlesNumberLists :: Test
-- handlesNumberLists = TestCase (assertEqual "handles number list," 
--     (myLast [1, 2, 3, 4])
--     4)
-- 
-- handlesCharLists :: Test
-- handlesCharLists = TestCase (assertEqual "handle Char list,"
--     (myLast ['x', 'y', 'z'])
--     'z')
--     
-- Also works:
-- tests :: Test
-- tests = TestList [
--     TestLabel "handlesNumberLists" handlesNumberLists, 
--    TestLabel "handlesCharLists" handlesCharLists]
