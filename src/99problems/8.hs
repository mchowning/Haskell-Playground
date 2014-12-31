{-#OPTIONS_GHC -Wall -Werror #-} 

import Test.HUnit

--------------------------------------------------
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element.  The order of the elements should not be changed.
--------------------------------------------------


-- linear recursive process
myCompress1 :: (Eq a) => [a] -> [a]
myCompress1 [] = []
myCompress1 [x] = [x]
myCompress1 (x:y:xs) | x == y    = myCompress1 $ y:xs
                     | otherwise = x : myCompress1 (y:xs)


-- linear iterative process starting at the front
myCompress2 :: (Eq a) => [a] -> [a]
myCompress2 [] = []
myCompress2 (x:xs) = myCompress2' [x] xs
    where
        myCompress2' :: (Eq a) => [a] -> [a] -> [a]
        myCompress2' result [] = result
        myCompress2' out (y:ys) | last out == y = myCompress2' out ys
                                | otherwise     = myCompress2' (out ++ [y]) ys


-- linear iterative process starting at the back
myCompress3 :: (Eq a) => [a] -> [a]
myCompress3 [] = []
myCompress3 xs = myCompress3' (init xs) [last xs]
    where
        myCompress3' :: (Eq a) => [a] -> [a] -> [a]
        myCompress3' [] result = result
        myCompress3' orig result | last orig == head result = myCompress3' (init orig) result
                                 | otherwise = myCompress3' (init orig) (last orig : result)

            


--------------------------------------------------
-- Tests
--------------------------------------------------

-- To execute tests run "runTestTT tests" in the ghci terminal

check :: (String -> String) -> Test
check f = TestCase (assertEqual "generic test"
    "abcade"
    (f "aaaabccaadeeee"))

testableMethods :: (Eq a) => [[a] -> [a]]
testableMethods = [myCompress1, myCompress2, myCompress3]

tests :: Test
tests = TestList (map check testableMethods)


