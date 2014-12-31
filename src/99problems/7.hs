{-#OPTIONS_GHC -Wall -Werror #-} 

import Test.HUnit

--------------------------------------------------
-- Flatten a nested list structure.  Transform a list, possibly holding lists as elements, into a 'flat' list by replacing each list with its elements (recursively)
--------------------------------------------------

-- This requires a new data type because Haskell lists are homogenous
data NestedList a = Elem a | List [NestedList a]

myFlatten1 :: NestedList a -> [a]
myFlatten1 =  

TODO


--------------------------------------------------
-- Tests
--------------------------------------------------

-- To execute tests run "runTestTT tests" in the ghci terminal

singleElem :: (Show a, Eq a, Num a) => (NestedList a -> [a]) -> Test
singleElem f = TestCase (assertEqual "handles a single element listk,"
    [5]
    (f (Elem 5)))

multipleElem :: (Show a, Eq a, Num a) => (NestedList a -> [a]) -> Test
multipleElem f = TestCase (assertEqual "message"
    [1, 2, 3, 4, 5]
    (f (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])))

--handlesEmptyList :: (NestedList a -> [a]) -> Test

testableMethods :: [NestedList a -> [a]]
testableMethods = [myFlatten1]

tests :: Test
tests = TestList (map singleElem testableMethods ++
                  map multipleElem testableMethods)


