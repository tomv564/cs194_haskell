module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
-- fibs2 = [0,1] ++ [ (fibs2 !! n - 2) + (fibs2 !! n - 1) | n <- [2..] ] not lazy!
fibs2 = scanl (+) 0 (1:fibs2)

-- definition of List:
--data List a = Nil | Cons a (List a)
--    deriving Show

-- the same, without the Nil option.
data Stream a = Cons a (Stream a)

-- where
--     : => a -> Stream a -> Stream case

streamToList :: Stream a -> [a]
streamToList (Cons n s) = n : streamToList s


instance Show a => Show (Stream a) where
  show s = show $ take 20 (streamToList s)

-- exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons n s) = Cons (f n) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)

-- exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap greatestPower positiveInts
    where
        greatestPower x = maximum $ takeWhile (\n -> (x `mod` 2^n) == 0) [0..x]
        positiveInts = streamFromSeed (+1) 1

-- 1,2,3,4,5,6,7,8,9,
-- 0,1,0,2,0,1,0,3,0


