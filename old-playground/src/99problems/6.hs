{-#OPTIONS_GHC -Wall -Werror #-} 

import Test.HUnit

--------------------------------------------------
-- Find out whether a list is a palindrome (i.e., xamax)
--------------------------------------------------


-- pattern matching
isPalindrome1 :: (Eq a) => [a] -> Bool
isPalindrome1 [] = True
isPalindrome1 [_] = True
isPalindrome1 (x:xs) = x == last xs && isPalindrome1 (init xs)


-- guards
isPalindrome2 :: (Eq a) => [a] -> Bool
isPalindrome2 xs 
    | length xs < 2 = True
    | otherwise     = (head xs == last xs) && isPalindrome2  (tail $ init xs)


--------------------------------------------------
-- Tests
--------------------------------------------------

-- To execute tests run "runTestTT tests" in the ghci terminal

properlyFails :: ([Int] -> Bool) -> Test
properlyFails f = TestCase (assertEqual "should fail on [1, 2, 3],"
    False
    (f [1, 2, 3]))

handlesString :: (String -> Bool) -> Test
handlesString f = TestCase (assertEqual "madamimadam is a palindrome,"
    True
    (f "madamimadam"))

handlesNumList :: ([Int] -> Bool) -> Test
handlesNumList f = TestCase (assertEqual "should handle number palindrome list,"
    True
    (f [1, 2, 4, 8, 16, 8, 4, 2, 1]))

testableMethods :: (Eq a) => [[a] -> Bool]
testableMethods = [isPalindrome1, isPalindrome2]

tests :: Test
tests = TestList (map properlyFails testableMethods ++
                  map handlesString testableMethods ++
                  map handlesNumList testableMethods)


