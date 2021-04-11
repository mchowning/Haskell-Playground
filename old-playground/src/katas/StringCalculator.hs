{-#OPTIONS_GHC -Wall -Werror #-}

import Test.HUnit
-- import Data.List.Split

data Op = Value String
        | Op (Double -> Double -> Double) Op Op

data Paren = PStart
           | PEnd
           | None

processOp :: Op -> Double
processOp (Value s) = read s
processOp (Op f l r) = f (processOp l) (processOp r)

stringCalc :: String -> Double
stringCalc = processOp . stringProcessor . words

stringProcessor :: [String] -> Op
stringProcessor [] = undefined
stringProcessor [x] = Value x
stringProcessor [_,_] = undefined
stringProcessor (x:xs) = stringProcessorWithLeft (Value x) xs

stringProcessorWithLeft :: Op -> [String] -> Op
stringProcessorWithLeft lo [] = lo
stringProcessorWithLeft lo (o:x:xs)
  | head x == '(' = extendLowerRight (tail x)
--   | head x == '(' = let splits = splitWhen (\a -> last a == ')') xs -- problem, this drops the delimiters
--                         end = last splits
--                         rest = concat . init $ splits
--                     in stringProcessorWithLeft (Op (getOp o) lo (stringProcessor rest)) end
  | otherwise     = extendUpperRight
  where
    extendLowerRight subx = Op (getOp o) lo (stringProcessorWithLeft (Value subx) xs)
    extendUpperRight = stringProcessorWithLeft (Op (getOp o) lo (Value x)) xs
stringProcessorWithLeft _ _ = undefined

-- stringCalc :: String -> Double
-- stringCalc s = let (x:xs) = words s
--                in stringPartCalc (read x) xs
--   where
--     stringPartCalc :: Double -> [String] -> Double
--     stringPartCalc n (o:x:xs) = stringPartCalc (getOp o n (read x)) xs
--     stringPartCalc n _ = n

getOp :: String -> Double -> Double -> Double
getOp "+" = (+)
getOp "-" = (-)
getOp "*" = (*)
getOp "/" = (/)
getOp _ = undefined

readDigit :: String -> (Double, Paren)
readDigit s | head s == '(' = (read . tail $ s, PStart)
            | last s == ')' = (read . init $ s, PEnd)
            | otherwise     = (read s, None)


tests :: IO Counts
tests = runTestTT (TestList [
  "Basic addition" ~: stringCalc "1 + 12" ~?= 13
  , "Basic subtraction" ~: stringCalc "50 - 1" ~?= 49
  , "Basic multiplication" ~: stringCalc "5 * 11" ~?= 55
  , "Basic division" ~: stringCalc "3 / 4" ~?= 0.75
  , "Longer equation" ~: stringCalc "2 + 8 - 4" ~?= 6
  , "Longer equation" ~: stringCalc "2 + 8 - 4 / 10" ~?= 0.6
  , "Simple parentheses" ~: stringCalc "2 + (6 / 2" ~?= 5
  , "Simple parentheses" ~: stringCalc "2 + (6 / 2)" ~?= 5
  ])