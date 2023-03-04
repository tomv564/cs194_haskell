module Main where

import Control.Monad.Random
import Risk

main :: IO ()
main = do
    res <- evalRandIO (pure bf >>= successProb)
    print $ show res
      where bf = Battlefield 5 5