module Golf where

import Data.List

skips :: [a] -> [[a]]
skips [] = []
skips (x:xs) = [x:xs] ++ skips xs
-- something with zip xs [1..] and mod

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:d) | (a < b) && (b > c) = [b] ++ (localMaxima (c : d))
localMaxima (a:b:c:d) | otherwise = localMaxima (b : c : d)
localMaxima otherwise = []

histogram :: [Integer] -> String
histogram values = intercalate "\n" $ (reverse rows) ++ [axis, labels]
    where
        rows = map drawRow [1..height]
        drawRow rowNum = concatMap (\freq -> draw freq rowNum) freqs
        draw val rowNum = if val >= rowNum then "*" else " "
        height = maximum freqs
        freqs = map (\val -> length $ elemIndices val values) range
        axis =  concat $ take (length range) $ (repeat "=")
        labels = concatMap show range
        range = [0..9]
