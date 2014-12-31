{-#OPTIONS_GHC -Wall -Werror #-}

import Test.HUnit
import Control.Applicative

-------------------------------------------------------------------------
-- Using folds, roll your own implementation of product
-------------------------------------------------------------------------

myProduct1 :: Num a => [a] -> a
myProduct1 = foldr (*) 1

myProduct2 :: Num a => [a] -> a
myProduct2 = foldl (*) 1

myProduct3 :: Num a => [a] -> a
myProduct3 [] = 0
myProduct3 xs = foldr1 (*) xs

--------------------------------------------------

productTestTemplate :: String -> [Int] -> Int -> ([Int] -> Int) -> Test
productTestTemplate message input expected testedF= TestCase (assertEqual message
    expected
    (testedF input))

productTestableMethods :: (Num a) => [[a] -> a]
productTestableMethods = [product, myProduct1, myProduct2, myProduct3]

productTestMethods :: [([Int] -> Int) -> Test]
productTestMethods = [productTestTemplate "[1, 2, 3, 4, 5]" [1, 2, 3, 4, 5] 120,
                      productTestTemplate "[4, 5, 7]" [4, 5, 7] 140]

productTests :: Test
productTests = TestList (productTestMethods <*> productTestableMethods)



-------------------------------------------------------------------------
-- Using folds, roll your own implementation of all
-------------------------------------------------------------------------

myAll1 :: (a -> Bool) -> [a] -> Bool
myAll1 f = foldr (\input bool -> f input == bool) True

myAll2 :: (a -> Bool) -> [a] -> Bool
myAll2 f = foldl (\bool input -> f input == bool) True

--------------------------------------------------

allTestTemplate :: String -> [Int] -> (Int -> Bool) -> ((Int -> Bool) -> [Int] -> Bool) -> Test
allTestTemplate message list predicateF testedF  = TestCase (assertEqual message
    (all predicateF list)
    (testedF predicateF list))

allTestableMethods :: [(a -> Bool) -> [a] -> Bool]
allTestableMethods = [myAll1, myAll2]

allTestMethods :: [((Int -> Bool) -> [Int] -> Bool) -> Test]
allTestMethods = [allTestTemplate "[5, 5, 5]" [5, 5, 5] (== 5), 
                  allTestTemplate "[5, 5, 4]" [5, 5, 4] (== 5),
                  allTestTemplate "[5, 10, 15]" [5, 10, 15] (\x -> mod x 5 == 0)]

allTests :: Test
--allTests = TestList (allTestMethods <*> allTestableMethods)
allTests = TestList ([ x y | x <- allTestMethods, y <- allTestableMethods])


-------------------------------------------------------------------------
-- Using folds, roll your own implementation of and
-------------------------------------------------------------------------

myAnd1 :: [Bool] -> Bool
myAnd1 = foldr (==) True

myAnd2 :: [Bool] -> Bool
myAnd2 = foldr (==) True

myAnd3:: [Bool] -> Bool
myAnd3 [] = True
myAnd3 xs = foldr1 (&&) xs

--------------------------------------------------

andTestTemplate :: String -> [Bool] -> Bool -> ([Bool] -> Bool) -> Test
andTestTemplate message list expected testMethod = TestCase (assertEqual message
    expected
    (testMethod list))

andTestableMethods :: [[Bool] -> Bool]
andTestableMethods = [and, myAnd1, myAnd2, myAnd3]

andTestMethods :: [([Bool] -> Bool) -> Test]
andTestMethods = [andTestTemplate "[TTT] == T" [True, True, True] True,
                  andTestTemplate "[TFT] == F" [True, False, True] False,
                  andTestTemplate "[] == T" [] True]

andTests :: Test
andTests = TestList (andTestMethods <*> andTestableMethods)



-------------------------------------------------------------------------
-- Using folds, roll your own implementation of or
-------------------------------------------------------------------------

myOr1 :: [Bool] -> Bool
myOr1 = foldr (||) False

myOr2 :: [Bool] -> Bool
myOr2 [] = False
myOr2 xs = foldr1 (||) xs

--------------------------------------------------

orTestTemplate :: String -> [Bool] -> Bool -> ([Bool] -> Bool) -> Test
orTestTemplate message list expected testMethod = TestCase (assertEqual message
    expected
    (testMethod list))

orTestableMethods :: [[Bool] -> Bool]
orTestableMethods = [or, myOr1, myOr2]

orTestMethods :: [([Bool] -> Bool) -> Test]
orTestMethods = [orTestTemplate "TTT == T" [True, True, True] True,
                 orTestTemplate "TFT == T" [True, False, True] True,
                 orTestTemplate "FFF == F" [False, False, False] False,
                 orTestTemplate "[] == F" [] False]

orTests :: Test
orTests = TestList (orTestMethods <*> orTestableMethods)


-------------------------------------------------------------------------
-- Using folds, roll your own implementation of concatMap
-------------------------------------------------------------------------

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = foldr (\x y -> f x ++ y) []

--------------------------------------------------

concatMapTestTemplate :: (Show b, Eq b) => String -> (a -> [b]) -> [a] -> [b] -> ((a -> [b]) -> [a] -> [b]) -> Test
concatMapTestTemplate message inputFunction inputList expected testMethod = TestCase (assertEqual message
    expected
    (testMethod inputFunction inputList))

concatMapTestableMethods :: [(a -> [b]) -> [a] -> [b]]
concatMapTestableMethods = [concatMap, myConcatMap]

concatMapTestMethods :: [((Int -> [Int]) -> [Int] -> [Int]) -> Test]
concatMapTestMethods = [concatMapTestTemplate "[1, 2, 3] -> [2, 3, 4]" 
                                              (\x -> [x + 1]) [1, 2, 3] [2, 3, 4],
                        concatMapTestTemplate ""
                                              (\x -> [x * x]) [3, 4, 6] [9, 16, 36]]

concatMapTests :: Test
concatMapTests = TestList (concatMapTestMethods <*> concatMapTestableMethods)



-------------------------------------------------------------------------
-- Roll your own implementation of scan
-------------------------------------------------------------------------

myScanl1 :: (b -> a -> b) -> b -> [a] -> [b]
myScanl1 _ n [] = [n]
myScanl1 f n (x:xs) = n : myScanl1 f (f n x) xs

myScanl2 :: (b -> a -> b) -> b -> [a] -> [b]
myScanl2 _ b [] = [b]
myScanl2 f b as = myScanl2 f b (init as) ++ [foldl f b as]

--------------------------------------------------

scanlTestTemplate :: (Show b, Eq b) => String -> (b -> a -> b) -> b -> [a] -> [b] -> ((b -> a -> b) -> b -> [a] -> [b]) -> Test
scanlTestTemplate message inputF b as expected testedF = TestCase (assertEqual message
    expected
    (testedF inputF b as))

scanlTestableMethods :: [(b -> a -> b) -> b -> [a] -> [b]]
scanlTestableMethods = [scanl, myScanl1, myScanl2]

scanlTestMethods :: [((Int -> Int -> Int) -> Int -> [Int] -> [Int]) -> Test]
scanlTestMethods = [scanlTestTemplate "100 [1, 2, 3] -> [100, 101, 103, 106]" 
                       (+) 100 [1, 2, 3] [100, 101, 103, 106]]

scanlTests :: Test
scanlTests = TestList (scanlTestMethods <*> scanlTestableMethods)

-------------------------------------------

type scanrType = (b -> a -> b) -> b -> [a] -> [b]


myScanr :: scanrType
myScanr = scanr


-----------------------

scanlTestTemplate :: (b -> a -> b) -> b -> [a] -> scanrType
scanlTestTemplate = 


scanrTestableMethods :: [scanrType]
scanrTestableMethods = [myScanr]

scanlTestMethods :: [(b -> a -> b) -> b -> [a] -> scanrType]
scanlTestMethods = []


------------------------------------------------------------------------
-- roll your own implementation of span
-------------------------------------------------------------------------


mySpan1 :: (a -> Bool) -> [a] -> ([a], [a])
mySpan1 f = mySpan1' f []
    where
        mySpan1' :: (a -> Bool) -> [a] -> [a] -> ([a], [a])
        mySpan1' _ ys [] = (ys, [])
        mySpan1' f' ys (z:zs)
            | f' z       = mySpan1' f' (ys ++ [z]) zs
            | otherwise = (ys, z:zs)

-- try using a more purely recurseive (as opposed to linear) process
--mySpan2 :: (a -> Bool) -> [a] -> ([a], [a])
--mySpan2 = 

mySpan3 :: (a -> Bool) -> [a] -> ([a], [a])
mySpan3 f xs = splitAt (getFailureIndex f xs) xs
    where
        getFailureIndex :: (a -> Bool) -> [a] -> Int
        getFailureIndex f' ys = getFailureIndex' 0 f' ys
            where
                getFailureIndex' :: Int -> (a -> Bool) -> [a] -> Int
                getFailureIndex' i _ [] = i + 1
                getFailureIndex' i f'' (z:zs) 
                                    | f'' z     = getFailureIndex' (i + 1) f'' zs
                                    | otherwise = i

--------------------------------------------------

spanTestTemplate :: (Show a, Eq a) => String -> (a -> Bool) -> [a] -> ([a], [a]) -> ((a -> Bool) -> [a] -> ([a], [a])) -> Test
spanTestTemplate message inputF list expected testedF = TestCase (assertEqual message
    expected
    (testedF inputF list))

spanTestableMethods :: [(a -> Bool) -> [a] -> ([a], [a])]
spanTestableMethods = [span, mySpan1, mySpan3]

spanTestMethods :: [((Int -> Bool) -> [Int] -> ([Int], [Int])) -> Test]
spanTestMethods = [spanTestTemplate "(<4) [2..7] ([2, 3], [4, 5, 6, 7])" 
                        (<4) [2..7] ([2, 3], [4, 5, 6, 7])]

spanTests :: Test
spanTests = TestList (spanTestMethods <*> spanTestableMethods)


-------------------------------------------------------------------------
-- Using interact, write a "toy" command line program
-------------------------------------------------------------------------




--------------------------------------------------




