--{-#OPTIONS_GHC -Wall -Werror #-}

import Test.HUnit
import Control.Monad.State

scoreGame :: [Int] -> Int
scoreGame = scoreFrame 0

scoreFrame :: Int -> [Int] -> Int
scoreFrame 10 _ = 0
scoreFrame frame rolls
  | isStrike  = strikeSpareScore + scoreFrame (frame+1) (drop 1 rolls)
  | isSpare   = strikeSpareScore + scoreFrame (frame+1) (drop 2 rolls)
  | otherwise = regularScore + scoreFrame (frame+1) (drop 2 rolls)
  where
    isStrike = head rolls == 10
    isSpare = regularScore == 10
    regularScore = sum . take 2 $ rolls
    strikeSpareScore = sum . take 3 $ rolls

scoreGame2 :: [Int] -> Int
scoreGame2 = fst . (!!10) . iterate scoreFrame2 . (,) 0

scoreFrame2 :: (Int, [Int]) -> (Int, [Int])
scoreFrame2 (total,rolls)
  | isStrike  = (total + strikeSpareScore, drop 1 rolls)
  | isSpare   = (total + strikeSpareScore, drop 2 rolls)
  | otherwise = (total + regularScore, drop 2 rolls)
  where
    isStrike = head rolls == 10
    isSpare = regularScore == 10
    regularScore = sum . take 2 $ rolls
    strikeSpareScore = sum . take 3 $ rolls

tests :: IO Counts
tests = runTestTT (TestList [
    "Empty game"        ~: scoreGame (replicate 20 0)                         ~?= 0
  , "Roll all 1s"       ~: scoreGame (replicate 20 1)                         ~?= 20
  , "Roll spare"        ~: scoreGame ([4,6,3,2] ++ replicate 16 0)            ~?= 18
  , "Roll strike"       ~: scoreGame ([10,5,3,7] ++ replicate 16 0)           ~?= 33
  , "Spare then Strike" ~: scoreGame ([3,2,10,6,4,3,1] ++ replicate 12 0)     ~?= 42
  , "Strike then Spare" ~: scoreGame ([10,2,8,6,2] ++ replicate 14 0)         ~?= 44
  , "Perfect game"      ~: scoreGame (replicate 12 10)                        ~?= 300
  ])



-- from web:

-- import Control.Monad.State

-- score = sum . evalState (replicateM 10 (State scoreFrame))
--   where scoreFrame (10:rest@(a:b:_)) = (10+a+b,rest)
--         scoreFrame (x:y:rest) | x+y < 10 = (x+y,rest)
--                               | otherwise = (10+head rest,rest)
