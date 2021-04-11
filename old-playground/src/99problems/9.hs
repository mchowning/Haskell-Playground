{-#OPTIONS_GHC -Wall -Werror #-} 

import Test.HUnit
import Data.List

--------------------------------------------------
-- Pack consecutive duplicates of list elements into sublists.  If a list contains repeated elements they should be placed in separate sublists.
--------------------------------------------------

packer :: (Eq a) => [a] -> [[a]]
-- packer [] = []
-- packer x:xs = packer' [[x]] xs
--     where
--         packer' res (x:xs) | 
packer = group



--------------------------------------------------
-- Tests
--------------------------------------------------

-- To execute tests run "runTestTT tests" in the ghci terminal

myTest :: (String -> [String]) -> Test
myTest f = TestCase (assertEqual "generic test"
    ["aaaa", "b", "cc", "aa", "d", "eeee"]
    (f "aaaabccaadeeee"))

testableMethods :: (Eq a) => [[a] -> [[a]]]
testableMethods = [packer]

tests :: Test
tests = TestList (map myTest testableMethods)


