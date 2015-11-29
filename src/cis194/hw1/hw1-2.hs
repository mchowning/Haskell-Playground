{-#OPTIONS_GHC -Wall -Werror #-}

import Data.Char
import Test.HUnit

tests :: IO Counts
tests = runTestTT . TestList $
  toDigitsTests ++
  toDigitsRevTests ++
  doubleEveryOtherTests ++
  sumDigitsTests ++
  validateTests ++
  hanoiTests ++
  hanoi4Tests

---------------------- Exercise 1 ----------------------

toDigits :: Integer -> [Integer]
toDigits x | x <= 0    = []
           | otherwise = map (fromIntegral . digitToInt) $ show x

toDigitsTests :: [Test]
toDigitsTests = [ "toDigits 1234"  ~:
                   toDigits 1234  ~?= [1,2,3,4]
                , "toDigits 0"   ~:
                   toDigits 0     ~?= []
                , "toDigits -17" ~:
                   toDigits (-17) ~?= []
                ]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

toDigitsRevTests :: [Test]
toDigitsRevTests = [ "toDigitsRev 1234" ~:
                      toDigitsRev 1234  ~?= [4,3,2,1]
                   , "toDigitsRev 0" ~:
                      toDigitsRev 0     ~?= []
                   , "toDigitsRev -17" ~:
                      toDigitsRev (-17) ~?= []
                   ]

---------------------- Exercise 2 ----------------------

doubleEveryOtherFromRight :: [Integer] -> [Integer]
doubleEveryOtherFromRight  = reverse . doubleIfEvenIndex . reverse
  where doubleIfEvenIndex :: [Integer] -> [Integer]
        doubleIfEvenIndex = zipWith ($) (cycle [id,(*2)]) --- !!!
{-        doubleIfEvenIndex = map (\(a,b) -> if isEven a then 2*b else b) . zip [1,2..]
        where isEven :: Integer -> Bool
              isEven x = rem x 2 == 0-}

doubleEveryOtherTests :: [Test]
doubleEveryOtherTests = [ "doubleEveryOtherFromRight [8,7,6,5]" ~:
                           doubleEveryOtherFromRight [8,7,6,5] ~?= [16,7,12,5]
                        , "doubleEveryOtherFromRight [1,2,3]"   ~:
                           doubleEveryOtherFromRight [1,2,3]   ~?= [1,4,3]
                        ]

---------------------- Exercise 3 ----------------------

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

sumDigitsTests :: [Test]
sumDigitsTests = [ "sumDigits [16,7,12,5]" ~: sumDigits [16,7,12,5] ~?= 22 ]

---------------------- Exercise 4 ----------------------

validate :: Integer -> Bool
-- validate = (== 0) . flip rem 10 . sumDigits . doubleEveryOtherFromRight . toDigits
-- validate = (== 0) . flip rem 10 . checkSumValue
validate n = checkSumValue n `isDivisibleBy` 10
  where checkSumValue :: Integer -> Integer
        checkSumValue = sumDigits . doubleEveryOtherFromRight . toDigits
        isDivisibleBy :: Integer -> Integer -> Bool
        isDivisibleBy x y = rem x y == 0

validateTests :: [Test]
validateTests = [ "validate 4012888888881881" ~: validate 4012888888881881 ~?= True
                , "validate 4012888888881882" ~: validate 4012888888881882 ~?= False
                ]

---------------------- Exercise 5 ----------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _           = []
hanoi n start end other = hanoi (n-1) start other end ++
                          [(start, end)] ++
                          hanoi (n-1) other end start

hanoiTests :: [Test]
hanoiTests = [ "hanoi 2" ~: hanoi 2 "a" "b" "c" ~?= [("a","c"), ("a","b"), ("c","b")]
             , "hanoi 3" ~: hanoi 3 "a" "b" "c" ~?= [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
             , "hanoi 15 length" ~: length (hanoi 15 "a" "b" "c") ~?= 32767
             ]

---------------------- Exercise 6 ----------------------

-- TODO not finished

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _                 = []
hanoi4 1 start end _ _           = [(start,end)]
hanoi4 2 start end other1 _      = [(start,other1), (start,end), (other1,end)]
hanoi4 3 start end other1 other2 = [(start,other1), (start,other2), (start,end), (other2,end), (other1,end)]
-- hanoi4 n start end other1 other2 =
hanoi4 _ _ _ _ _                 = []

hanoi4Tests :: [Test]
hanoi4Tests = [ --"hanoi4 3 (is still valid if c and d reversed)" ~: hanoi4 3 "a" "b" "c" "d" ~?= [("a","c"), ("a","d"), ("a","b"), ("d","b"), ("c","b")]
--               , "hanoi4 15 length" ~: length (hanoi4 15 "a" "b" "c" "d") ~?= 129
              ]

