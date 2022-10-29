{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ApplicativeDo #-}
module Main where

import Control.Monad.Trans.State.Lazy 
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty as NonEmpty
import GHC.Exts (IsList)
import Data.Ratio ((%))
import Data.Map.Strict as Map
import Data.Set as Set
import Data.List as List
import Text.Show.Pretty

main :: IO ()
main = do 
  putStrLn "Hello, Haskell!"

data Possibility a = Possibility {
  outcome :: a,
  weight :: Rational
} deriving (Show)

-- Invariant: sum of weights is 1
newtype Distribution a = Distribution {
  possibilities :: NonEmpty (Possibility a)
} deriving newtype (IsList)

-- Just make it easier to read
instance Show a => Show (Distribution a) where
  show distribution = "Distribution " <> show (NonEmpty.toList (possibilities distribution))

-- Enforces invariant that Distribution weights sum to 1
createDistribution :: NonEmpty (Possibility a) -> Distribution a
createDistribution ps = 
  let totalWeight = sum (NonEmpty.map weight ps)
  in if totalWeight == 1
    then Distribution ps 
    else error ("Total weight of " <> show totalWeight <> " is not 1")

instance Functor Possibility where
  fmap f (Possibility outcome' weight') = Possibility (f outcome') weight'

instance Applicative Possibility where
  pure a = Possibility a 1
  fab <*> fa = 
    let b = outcome fab (outcome fa)
        weight' = weight fab * weight fa
    in Possibility b weight'
  
-- test :: Bool -> Possibility Bool 
-- test c = do
--   a <- Possibility True 0.75
--   b <- Possibility False 0.25
--   pure (a == b)

instance Functor Distribution where
  fmap f (Distribution ps) = Distribution (NonEmpty.map (fmap f) ps)

instance Applicative Distribution where
  pure a = Distribution (NonEmpty.fromList [pure a])
  fab <*> fa =
    let fabp = possibilities fab
        ap = possibilities fa 
        -- b = _ <*> ap
        b = do
          a <- ap
          f <- fabp
          pure (f <*> a)
        -- weight' = possibilities fab <*> possibilities fa
    in Distribution b

test :: Distribution (Set Bool)
test = combine $ do
  t <- createDistribution (NonEmpty.fromList [Possibility True 0.4, Possibility False 0.6])
  f <- createDistribution (NonEmpty.fromList [Possibility True 0.1, Possibility False 0.9])
  pure $ Set.fromList (List.sort [t, f])

combine :: Ord a => Distribution a -> Distribution a
combine (Distribution ps) = 
  let 
    -- Sort by outcome
    sorted = sortWith outcome ps
    -- Group by outcome
    grouped = NonEmpty.groupWith1 outcome sorted
    -- Sum weights
    summed = (\ps' -> Possibility (outcome (NonEmpty.head ps')) (weightSum ps')) <$> grouped
  in Distribution summed
    where
      weightSum = sum . NonEmpty.map weight
