module Main where

import Employee
import Party

main :: IO ()
main = do
    file <- readFile "company.txt"
    let (GL emps f) = maxFun $ read file
        in
            (putStrLn $ "Total fun: " ++ (show f)) >> putStrLn (getNames emps)
