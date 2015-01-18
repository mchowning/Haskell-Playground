{-#OPTIONS_GHC -Wall -Werror #-}

import Data.Char
import Test.QuickCheck
import Test.HUnit

--- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\i acc -> (i - 2) * acc) 1 . filter even

-- QuickCheck property
prop_fun1 :: [Integer] -> Bool
prop_fun1 a = fun1 a == fun1' a

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate funHelper
  where funHelper x | even x    = div x 2
                    | otherwise = 3 * x + 1

-- QuickCheck property
-- Have to limit the input to numbers above 0 so the original function doesn't enter an infinite loop
prop_fun2 :: Integer -> Property
prop_fun2 a = a > 0 ==> fun2 a == fun2' a


--- Exercise 2

data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- todo must be a better way

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree [x] = Node 0 Leaf x Leaf
foldTree (x:xs) = let (l,r) = splitAt (length xs `div` 2) xs
                      left = foldTree l
                      right = foldTree r
                      depth = fromIntegral (maximum [getDepth left, getDepth right])
                  in Node depth left x right


-- Testing stuff --

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ l _ r) = abs (getDepth l - getDepth r) <= 1

isBalancedTests :: Test
isBalancedTests =
  TestList [ isBalanced Leaf                                ~?= True
           , isBalanced anEmptyNode                         ~?= True
           , isBalanced (Node 0 anEmptyNode "" Leaf)        ~?= True
           , isBalanced (Node 0 Leaf "" anEmptyNode)        ~?= True
           , isBalanced (Node 0 anEmptyNode "" anEmptyNode) ~?= True
           , isBalanced (Node 0 Leaf "" (Node 0 Leaf "" anEmptyNode)) ~?= False
           , isBalanced (Node 0 Leaf "" (Node 0 anEmptyNode "" (Node 0 Leaf "" anEmptyNode))) ~?= False ]
    where anEmptyNode = Node 0 Leaf "" Leaf

getDepth :: Tree a -> Int
getDepth Leaf = 0
getDepth (Node _ l _ r) = 1 + maximum [getDepth l, getDepth r]

getDepthTests :: Test
getDepthTests =
  TestList [ getDepth Leaf ~?= 0
           , getDepth anEmptyNode ~?= 1
           , getDepth (Node 0 Leaf "" anEmptyNode) ~?= 2
           , getDepth (Node 0 Leaf "" (Node 0 Leaf "" anEmptyNode)) ~?= 3
           , getDepth (Node 0 Leaf "" (Node 0 Leaf "" (Node 0 Leaf "" anEmptyNode))) ~?= 4 ]
    where anEmptyNode = Node 0 Leaf "" Leaf

prop_foldTreeBalanced :: String -> Bool
prop_foldTreeBalanced = isBalanced . foldTree


--- Exercise 3.1

xor :: [Bool] -> Bool
xor = foldr helper False
  where
    helper :: Bool -> Bool -> Bool
    helper b acc | b         = not acc
                 | otherwise = acc

xorTests :: Test
xorTests =
  TestList [ xor []                   ~?= False
           , xor [True]               ~?= True
           , xor [False]              ~?= False
           , xor [True, True]         ~?= False
           , xor [True, False]        ~?= True
           , xor [False, True]        ~?= True
           , xor [True, True, True]   ~?= True
           , xor [True, True, False]  ~?= False
           , xor [False, False, True] ~?= True
           ]


--- Exercise 3.2

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

prop_mapInt :: [Int] -> Bool
prop_mapInt x = map (*3) x == map' (*3) x

prop_mapStr :: String -> Bool
prop_mapStr s = map toUpper s == map' toUpper s


--- Exercise 3.3


-- just use reverse on the list and flip on the function parameter?


--- Exercise 4



