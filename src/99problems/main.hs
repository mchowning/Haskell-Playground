{-#OPTIONS_GHC -Wall -Werror #-} 

import Data.List
import Test.HUnit

----------------------------------------------------------
-- Problem 9 
-- "aaaabbbbaccccc" -> ["aaaa", "bbbb", "a", "ccccc"]
----------------------------------------------------------


----------------------------------------------------------
-- Problem 10
----------------------------------------------------------

--type RunLengthT a = [a] -> [(Int, a)]

runLength :: (Eq a) => [a] -> [(Int, a)]
runLength = map f . group 
    where
        f m = (length m, head m)

--runLength as = map (\x -> (length x, head x)) (group as)

-- TESTS --

runLengthTest :: Test
runLengthTest = TestCase (assertEqual ""
    [(4, 'a'), (2, 'b'), (1, 'd')]
    (runLength "aaaabbd"))

----------------------------------------------------------
-- Problem 11   
----------------------------------------------------------

-- data Counter a = Single a | Multiple Int a deriving (Show, Eq)
data Counter a = Single {counterValue :: a} | 
                 Multiple {counterQuantity :: Int
                          , counterValue :: a
                          } deriving (Show, Eq)

encodeRunCounter :: (Eq a, Show a) => [a] -> [Counter a]
encodeRunCounter [] = []
encodeRunCounter xs = map f (runLength xs)
    where
        f (n,c) | n > 1 = Multiple n c
                | n == 1 = Single c
                | otherwise = undefined

-- TESTS --

runCounterTest :: Test
runCounterTest = TestCase(assertEqual ""
    [Multiple 4 'a', Multiple 2 'b', Single 'd']
    (encodeRunCounter "aaaabbd"))

----------------------------------------------------------
-- Problem 12
----------------------------------------------------------

decodeRunCounter :: [Counter a] -> [a]
decodeRunCounter = concatMap decoder 
    where
        decoder :: Counter a -> [a]
        decoder (Single a)      = [a]
        decoder (Multiple n a)  = replicate n a

-- TESTS --

decodeRunCounterTest :: Test
decodeRunCounterTest = TestCase (assertEqual ""
    "aaaabccaadeeee"
    (decodeRunCounter runCounterInput))
        where
            runCounterInput = [Multiple 4 'a', 
                               Single 'b', 
                               Multiple 2 'c', 
                               Multiple 2 'a', 
                               Single 'd', 
                               Multiple 4 'e']


----------------------------------------------------------
-- Problem 13
----------------------------------------------------------

directEncodeRunCounter :: (Eq a, Show a) => [a] -> [Counter a]
directEncodeRunCounter = foldr helper [] 
    where
        helper :: (Eq a, Show a) => a -> [Counter a] -> [Counter a]
        helper a [] = [Single a]
        helper a list@(x:_) | counterValue x == a = iterCounter x : tail list 
                            | otherwise           = Single a : list
            where
                iterCounter :: Counter a -> Counter a
                iterCounter Single {counterValue=value} = 
                    Multiple 2 value
                iterCounter Multiple {counterValue=value, counterQuantity=quantity} = 
                    Multiple (1 + quantity) value

-- TESTS --

directEncodeRunCounterTest :: (Show a, Eq a) => ([a], [Counter a]) -> Test
directEncodeRunCounterTest (input, result) = TestCase (assertEqual ""
    result
    (directEncodeRunCounter input))

directEncodeRunCounterTestList :: Test
directEncodeRunCounterTestList = TestList [
    directEncodeRunCounterTest ("abccaadeeee",
        [Single 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']),
    directEncodeRunCounterTest ("abccaadeeee",
        [Single 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']),
    directEncodeRunCounterTest ("", [])]


----------------------------------------------------------
-- Problem 14
----------------------------------------------------------

duplicator :: (Eq a, Show a) => [a] -> [a]
duplicator [] = []
duplicator (x:xs) = x:x:duplicator xs

-- TESTS --

duplicatorTest :: (Eq a, Show a) => [a] -> [a] -> Test
duplicatorTest input result = TestCase $ assertEqual "" result (duplicator input)

duplicatorTestList :: Test
duplicatorTestList = TestList [duplicatorTest "abc" "aabbcc"]


----------------------------------------------------------
-- Problem 15
----------------------------------------------------------

replicator :: (Eq a, Show a) => [a] -> Int -> [a]
--replicator xs n = concatMap (replicate n) xs
replicator = flip $ concatMap. replicate

-- TESTS --

replicatorTest :: (Eq a, Show a) => [a] -> Int -> [a] -> Test
replicatorTest inputList inputNum result = 
    TestCase (assertEqual "" result (replicator inputList inputNum))

replicatorTestList :: Test
replicatorTestList = TestList [replicatorTest "abbc" 3 "aaabbbbbbccc"]


----------------------------------------------------------
-- Problem 16
----------------------------------------------------------


