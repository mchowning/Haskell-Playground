{-#OPTIONS_GHC -Wall -Werror #-} 

import Test.HUnit

--------------------------------------------------
-- Find the last but one element of a list
--------------------------------------------------


myButLast1 :: [a] -> a
myButLast1 []        = undefined
myButLast1 [_]       = undefined
myButLast1 (x:y:xs)  | null xs = x
                    | otherwise = myButLast1 (y:xs)


myButLast2 :: [a] -> a
myButLast2 []       = undefined
myButLast2 [_]      = undefined
myButLast2 (x:xs)   | length xs == 1 = x
                    | otherwise = myButLast2 xs


myButLast3 :: [a] -> a
myButLast3 xs = 
    if length xs > 1
        then xs !! (length xs - 2)
        else undefined


myButLast4 :: [a] -> a
myButLast4 xs =
    let listLength = length xs
        butLastElement = xs !! (length xs - 2)
    in
        if listLength > 1
        then butLastElement
        else undefined


myButLast5 :: [a] -> a
myButLast5 xs =
    if listLength > 1
    then butLastElement
    else undefined
    where
        listLength = length xs
        butLastElement = xs !! (length xs - 2)


myButLast6 :: [a] -> a
myButLast6 = last . init

myButLast7 :: [a] -> a
myButLast7 = head . tail . reverse
        

--------------------------------------------------
-- Tests
--------------------------------------------------

-- To execute tests run "runTestTT tests" in the ghci terminal

handlesNumList :: ([Int] -> Int) -> Test
handlesNumList f = TestCase (assertEqual "handles num list"
    3
    (f [1, 2, 3, 4]))

handlesCharList :: (String -> Char) -> Test
handlesCharList f = TestCase (assertEqual "handle Char list"
    'y'
    (f "xyz"))

testableMethods :: [[a] -> a]
testableMethods = [myButLast1, myButLast2, myButLast3, myButLast4, myButLast5, myButLast6, myButLast7]

tests :: Test
tests = TestList (map handlesNumList testableMethods ++ 
                  map handlesCharList testableMethods)
