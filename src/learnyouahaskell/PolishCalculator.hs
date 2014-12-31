{-#OPTIONS_GHC -Wall -Werror #-}

--import Data.Char

-- FIRST ATTEMPT

--pCalc :: [String] -> Int
--pCalc = pCalc' []
--
--pCalc' :: [Int] -> [String] -> Int
--pCalc' [x] [] = x
--pCalc' ls (x:xs) | isNumberString x = pCalc' (((read x) :: Int):ls) xs
--                 | x == "+" = pCalc' ((b + a):cs) xs
--                 | x == "*" = pCalc' ((b * a):cs) xs
--                 | x == "-" = pCalc' ((b - a):cs) xs
--                 where (a:b:cs) = ls
--pCalc' _ _ = error "Invalid input"
--
--isNumberString :: String -> Bool
--isNumberString [] = True
--isNumberString (x:xs) | isNumber x = isNumberString xs
--                      | otherwise = False

-- SECOND ATTEMPT (using foldl)

--pCalc :: [String] -> Int
--pCalc = head . (foldl pCalc' [])
--
--pCalc' :: [Int] -> String -> [Int]
--pCalc' x [] = x
--pCalc' ls x | isNumberString x  = (read x):ls
--            | x == "+"          = (b + a):cs
--            | x == "*"          = (b * a):cs
--            | x == "-"          = (b - a):cs
--                 where (a:b:cs) = ls
--pCalc' _ _ = error "Invalid input"
--
--isNumberString :: String -> Bool
--isNumberString [] = True
--isNumberString (x:xs) | isNumber x = isNumberString xs
--                      | otherwise = False

-- THIRD ATTEMPT

--pCalc :: [String] -> Int
--pCalc = head . (foldl pCalc' [])
--
--pCalc' :: [Int] -> String -> [Int]
--pCalc' x [] = x
--pCalc' ls x | x == "+"          = (b + a):cs
--            | x == "*"          = (b * a):cs
--            | x == "-"          = (b - a):cs
--            | otherwise         = (read x):ls
--                 where (a:b:cs) = ls

-- FOURTH ATTEMPT

--pCalc :: String -> Int
--pCalc s = head (foldl pCalc' [] (splitOn " " s))
--
--pCalc' :: [Int] -> String -> [Int]
--pCalc' x [] = x
--pCalc' ls x | x == "+"          = (b + a):cs
--            | x == "*"          = (b * a):cs
--            | x == "-"          = (b - a):cs
--            | otherwise         = (read x):ls
--                 where (a:b:cs) = ls

-- FIFTH ATTEMPT

--pCalc :: String -> Int
--pCalc = head . foldl pCalc' [] . words
--
--pCalc' :: [Int] -> String -> [Int]
--pCalc' x [] = x
--pCalc' ls x | x == "+"          = (b + a):cs
--            | x == "*"          = (b * a):cs
--            | x == "-"          = (b - a):cs
--            | otherwise         = read x:ls
--                 where (a:b:cs) = ls

-- SIXTH ATTEMPT

pCalc :: (Num a, Read a) => String -> a
pCalc = head . foldl wordProcessor [] . words
    where
        wordProcessor :: (Num a, Read a) => [a] -> String -> [a]
        wordProcessor (x:y:ys) "+" = (y + x):ys
        wordProcessor (x:y:ys) "*" = (y * x):ys
        wordProcessor (x:y:ys) "-" = (y - x):ys
        wordProcessor ls s         = read s:ls

-- todo Use Maybe to make this fault tolerant
