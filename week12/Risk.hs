{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Functor.Identity (Identity)

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
die = getRandom

-- dice :: (RandomGen g) => Int -> Rand g [Int]
dice :: Int -> RandT StdGen Identity [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
    deriving (Show)

maxAttacking :: Battlefield -> Int
maxAttacking bf = min (attackers bf - 1) 3

maxDefending :: Battlefield -> Int
maxDefending bf = min (defenders bf) 2

casualty :: Battlefield -> (Int, Int) -> Battlefield
casualty (Battlefield at df) (da, dd)
                  | da > dd   = Battlefield at (df - 1)
                  | otherwise = Battlefield (at - 1) df


-- sortDesc :: Ordering a => [a] -> [a]
sortDesc :: [Int] -> [Int]
sortDesc = sortBy (flip compare)

-- a single battle:
-- roll dice, each player attacks with maximum allowed, update casualties.
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
    attacks <- roll (maxAttacking bf)
    defends <- roll (maxDefending bf)
    return $ foldl casualty bf (zip attacks defends)
    where
        roll n = sortDesc <$> map unDV <$> dice n

-- Exercise 3

-- repeated attacks
invade :: Battlefield -> Rand StdGen Battlefield
invade bf | attackers bf < 2 || defenders bf < 1 = return bf
          | otherwise = battle bf >>= invade

-- Exercise 4

successProb :: Battlefield -> Rand StdGen Double
successProb bf = calcStats <$> replicateM 1000 (invade bf)
  where
    calcStats xs = fromIntegral (countWins xs) / 1000
    countWins xs = length $ filter (\x -> defenders x < 1) xs

