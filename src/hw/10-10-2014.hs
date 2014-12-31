{-#OPTIONS_GHC -Wall -Werror #-}

fst' :: (a, b) -> a
fst' (a, _) = a

snd' :: (a, b) -> b
snd' (_, b) = b

even' :: Integral a => a -> Bool
even' a
    | a `mod` 2 == 0    = True    -- could have done mod x 2 == 0 instead
    | otherwise         = False

even'' :: Integral a => a -> Bool
even'' a = a `mod` 2 == 0


odd' :: Integral a => a -> Bool
odd' a
    | a `mod` 2 == 1    = True    --  | mod a 2 == 1    = True
    | otherwise         = False

odd'' :: Integral a => a -> Bool
odd'' a = not $ even' a


--------------------------------------------------


fac :: Int -> Int
fac 0 = 1
fac x | x < 0       = undefined
      | otherwise   = x * x'
    where
        x' = fac $ x - 1

-- main :: IO ()
-- main = do
--     putStr "Enter a number: "
--     x <- readLn                  -- what does this arrow do?
--     putStrLn . show . fac $ x    -- why the $?

-- main :: IO ()
-- main = do
--     putStr "Enter a number: "
--     x <- readLn
--     print . fac $ x    -- hlint improvement

-- Use where clause        
main :: IO ()
main = do
    prompt
    x <- readLn
    showFac x
  where
    prompt  = putStr "Enter a number: "
    showFac = putStrLn . show . fac
  
