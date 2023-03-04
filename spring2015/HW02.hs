{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c g = length $ filter (==True) (zipWith (==) c g)

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times it occurs in ys
countColors :: Code -> [Int]
countColors xs = map (\c -> length ( filter (==c) xs)) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ zipWith min (countColors xs) (countColors ys)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonexact
    where
        exact = exactMatches secret guess
        nonexact = matches secret guess - exact


-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move code =
        let
            (Move guess exact nonexact) = move
            (Move _ ex ne) = getMove code guess
            in exact == ex && nonexact == ne


-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes 1 = map (: []) colors
allCodes n = concatMap (\x -> map (: x) colors) (allCodes (n - 1))

-- Exercise 7 -----------------------------------------

tryCodes :: Code -> [Code] -> [Move]
tryCodes _ [] = []
tryCodes secret (x:xs) =
    if x == secret then
            [move]
        else
            move : tryCodes secret (filterCodes move xs)
    where move = getMove secret x

solve :: Code -> [Move]
solve secret =
    tryCodes secret (allCodes (length secret))


-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined

