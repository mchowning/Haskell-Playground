{-#OPTIONS_GHC -Wall -Werror #-}

import Data.Char

-----------------------------------------------------------------
-- Write a dropEveryThird function
-----------------------------------------------------------------

dropThird :: [a] -> [a]
dropThird (x:y:_:zs) = x : y : zs
dropThird xs            = xs

dropEveryThird :: [a] -> [a]
dropEveryThird []         = []
dropEveryThird [x]        = [x]
dropEveryThird [x, y]     = [x, y]
dropEveryThird (x:y:_:zs) = x : y : dropEveryThird zs

-----------------------------------------------------------------
-- Roll your own implementation of the standard last function
-----------------------------------------------------------------

last' :: [a] -> a
last' [] = undefined
last' [x] = x
last' (_:xs) = last' xs

-----------------------------------------------------------------
-- Roll your own implementation of the standard reverse function.  
-- Use init and last.
-----------------------------------------------------------------

reverse' :: [a] -> [a]
reverse' []  = [] -- why does it still work if I remove this line?
reverse' [x] = [x]
-- reverse' x = last x : (reverse' $ init x)
-- reverse' x = last x : reverse' (init x)
reverse' x = 
            start : end
          where
            start = last x
            end = reverse' $ init x

-----------------------------------------------------------------
-- Write a function that removes all upercase characters from a string.
-- Use isUpper.
-----------------------------------------------------------------

removeUpper :: [Char] -> [Char]
removeUpper []     = []
removeUpper (x:xs) | isUpper x = removeUpper xs
                   | otherwise = x : removeUpper xs

