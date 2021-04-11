{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

import Test.HUnit

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom -- see Control.Monad.Random.Class

------------------------------------------------------------
-- Risk

-- type Army = Int
type Attackers = Int
type Defenders = Int

data Battlefield = Battlefield { attackers :: Attackers
                               , defenders :: Defenders
                               } deriving (Eq, Show)


--- Exercise 2

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

maxAttackers :: Int -> Int
maxAttackers n = min 3 (n-1)

maxDefenders :: Int -> Int
maxDefenders = min 2

losses :: [DieValue] -- attacker die
       -> [DieValue] -- defender die
       -> (Attackers,Defenders)
losses ads dds = let rs = zip (reverse . sort $ ads) (reverse . sort $ dds)
                 in foldr addLoss (0,0) rs
  where
    addLoss :: (DieValue, DieValue) -> (Attackers, Defenders) -> (Attackers, Defenders)
    addLoss ((DV a),(DV d)) (al,dl)
      | a > d     = (al,dl+1)
      | otherwise = (al+1,dl)

updateBattlefield :: [DieValue] -> [DieValue] -> Battlefield -> Battlefield
updateBattlefield ads dds b =
  let battleLosses = losses ads dds
  in updateBattlefield' b battleLosses
    where
      updateBattlefield' :: Battlefield -> (Attackers,Defenders) -> Battlefield
      updateBattlefield' (Battlefield ats dfs) (al,dl) = Battlefield (ats-al) (dfs-dl)

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield ats dfs) = do attackerDie <- dice . maxAttackers $ ats
                                    defenderDie <- dice . maxDefenders $ dfs
                                    return $ updateBattlefield attackerDie defenderDie b

ex2Tests :: IO Counts
ex2Tests = runTestTT $ TestList
  [ TestLabel "losses" $ TestList
    [ losses [DV 2, DV 1, DV 5] [DV 4, DV 5] ~?= (2,0)
    , losses [DV 2, DV 1, DV 5] [DV 4, DV 4] ~?= (1,1)
    , losses [DV 6, DV 1, DV 5] [DV 4, DV 4] ~?= (0,2)
    ]
  , TestLabel "updateBattlefield" $ TestList
    [ updateBattlefield [DV 5, DV 3, DV 4] [DV 2, DV 5] (Battlefield 5 4) ~?= Battlefield 4 3
    , updateBattlefield [DV 5, DV 3, DV 4] [DV 5, DV 5] (Battlefield 5 4) ~?= Battlefield 3 4
    ]
  ]


--- Exercise 3

-- runs battles until there are either 0 defenders <2 attackers
invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield ats dfs)
 | ats < 2 || dfs == 0 = return b
 | otherwise           = do newB <- battle b
                            invade newB


--- Exercise 4

successProb :: Battlefield -> Rand StdGen Double
successProb b = do bs <- replicateM 1000 (invade b)
                   return $ successProb' bs

successProb' :: [Battlefield] -> Double
successProb' bs = let successes = foldr countSuccessfulAttacks 0 bs
                  in successes / fromIntegral (length bs)

countSuccessfulAttacks :: Num a => Battlefield -> a -> a
countSuccessfulAttacks (Battlefield _ dfs) n
  | dfs == 0  = n+1
  | otherwise = n


