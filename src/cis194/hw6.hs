{-#OPTIONS_GHC -Wall -Werror #-}


--- Exercise 1

-- basic fibonacci number calculator
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = n + fib (n - 1)

-- define infinte list of all fibonacci numbers
fibs1 :: [Integer]
fibs1 = map fib [1..]



