{-#OPTIONS_GHC -Wall -Werror #-} 

import Test.HUnit

--------------------------------------------------
-- Reverse a list
--------------------------------------------------


-- basic haskell function usage
myReverse1 :: [a] -> [a]
myReverse1 = reverse

-- linear recursive process
myReverse2 :: [a] -> [a]
myReverse2 [] = []
myReverse2 (x:xs)  = myReverse2 xs ++ [x]


-- linear iterative process
myReverse3 :: [a] -> [a]
myReverse3 = myReverse3' []
    where
        myReverse3' :: [a] -> [a] -> [a]
        myReverse3' rev [] = rev
        myReverse3' rev (x:xs) = myReverse3' (x : rev) xs


-- foldl...WTF black magic is this!?!?
myReverse4 :: [a] -> [a]
myReverse4 = myReverse4' []
    where
        myReverse4' :: [a] -> [a] -> [a]
        myReverse4' rev [] = rev
        myReverse4' rev xs = foldl (flip (:)) rev xs 
-- have to flip the (:) so it fits the (b -> a -> b) format of foldl (i.e., list -> item -> list)


--------------------------------------------------
-- Tests
--------------------------------------------------

-- To execute tests run "runTestTT tests" in the ghci terminal

handlesStrings :: (String -> String) -> Test
handlesStrings f = TestCase (assertEqual "handles Strings,"
    "!amanap ,lanac a ,nalp a ,nam A"
    (f "A man, a plan, a canal, panama!"))

handlesNumList :: ([Int] -> [Int]) -> Test
handlesNumList f = TestCase (assertEqual "handles Num lists,"
    [4, 3, 2, 1]
    (f [1, 2, 3, 4]))

testableMethods :: [[a] -> [a]]
testableMethods = [myReverse1, myReverse2, myReverse3, myReverse4]

tests :: Test
tests = TestList (map handlesStrings testableMethods ++
                  map handlesNumList testableMethods)


