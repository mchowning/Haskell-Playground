{-#OPTIONS_GHC -Wall -Werror #-}

import Test.QuickCheck

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


