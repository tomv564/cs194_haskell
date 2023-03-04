{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as SVM

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit n = (Lit n)
  add a b = (Add a b)
  mul a b = (Mul a b)

instance Expr Integer where
  lit n = n
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit n | n <= 0    = False
        | otherwise = True
  add a b = a || b
  mul a b = a && b

newtype MinMax  = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add expr1 expr2) = (eval expr1) + (eval expr2)
eval (Mul expr1 expr2) = (eval expr1) * (eval expr2)

evalStr :: String -> Maybe Integer
evalStr exprText = case parseExp Lit Add Mul exprText of
            Just expr -> Just (eval expr)
            Nothing -> Nothing

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr SVM.Program where
  lit n = [SVM.PushI n]
  add a b = a ++ b ++ [SVM.Add]
  mul a b = a ++ b ++ [SVM.Mul]


compile :: String -> Maybe SVM.Program
compile expr = parseExp lit add mul expr
