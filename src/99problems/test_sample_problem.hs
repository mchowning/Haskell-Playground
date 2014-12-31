{-#OPTIONS_GHC -Wall -Werror #-} 

testMethod :: a -> a
testMethod a = a

testMethod' :: (Show a, Eq a) => a -> a
testMethod' a = a

useTestMethod :: (Show a, Eq a, Num a) => (a -> a) -> a
useTestMethod f = f 5
