-- {-#OPTIONS_GHC -Wall -Werror #-}

import Data.Char
import Test.HUnit

sumStringDigits :: String -> Integer
sumStringDigits = foldr helper 0
  where
    helper :: Char -> Integer -> Integer
    helper c acc = (+ acc) . toInteger . digitToInt $ c

checksumStringDigits :: String -> Integer
checksumStringDigits s =  let rs = reverse s
                              ai = map (toInteger . digitToInt) rs
                              at = zip (cycle [False, True]) ai
                          in foldr helper 0 at
  where
    helper :: (Bool, Integer) -> Integer -> Integer
    helper (doubleIt, n) acc | doubleIt    =  acc + doubleCalculator n
                             | otherwise   =  acc + n

doubleCalculator :: Integer -> Integer
doubleCalculator n | n > 9 || n < 0 = undefined
                   | otherwise = let d = 2 * n
                                 in mod d 10 + div d 10


tests :: IO Counts
tests = runTestTT (TestList [
  sumStringDigits "3" ~?= 3
  , sumStringDigits "" ~?= 0
  , sumStringDigits "37" ~?= 10
  , sumStringDigits "307" ~?= 10
  , sumStringDigits "3075" ~?= 15

  , "4512113014843252" ~: checksumStringDigits "4512113014843252" ~?= 54
  , "4512113014643252" ~: checksumStringDigits "4512113014643252" ~?= 50

  , "0" ~: doubleCalculator 0 ~?= 0
  , "1" ~: doubleCalculator 1 ~?= 2
  , "2" ~: doubleCalculator 2 ~?= 4
  , "3" ~: doubleCalculator 3 ~?= 6
  , "4" ~: doubleCalculator 4 ~?= 8
  , "5" ~: doubleCalculator 5 ~?= 1
  , "6" ~: doubleCalculator 6 ~?= 3
  , "7" ~: doubleCalculator 7 ~?= 5
  , "8" ~: doubleCalculator 8 ~?= 7
  , "9" ~: doubleCalculator 9 ~?= 9

  ])