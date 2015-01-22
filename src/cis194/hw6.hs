{-#OPTIONS_GHC -Wall -Werror #-}

--- Exercise 1

-- basic fibonacci number calculator
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = n + fib (n - 1)

-- naive implementation of infinite list of all fibonacci numbers
fibs1 :: [Integer]
fibs1 = map fib [1..]


--- Exercise 2

fibs2 :: [Integer]
fibs2 = scanl1 (+) [1..]


--- Exercise 3

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = flip (++) "..." . show . take 10 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons i s) = i : streamToList s


--- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))


--- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 1

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a1 s1) s2 = Cons a1 (interleaveStreams s2 s1)

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [1..])
