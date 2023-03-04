{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P p1) == (P p2) = p1 == p2

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show  (P terms) = printTerms (length terms) (reverse terms)
        where
            printTerms _ [] = ""
            printTerms pos [x] = showTerm pos x
            printTerms pos (x:xs) = showTerm pos x ++ " + " ++ printTerms (pos - 1) xs
            showTerm 1 c = show c
            showTerm 2 c = show c ++ "x"
            showTerm p c = show c ++ "x^" ++ show (p - 1)

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p1) (P p2) = P (addCoefs p1 p2)
    where
    addCoefs [] [] = []
    addCoefs [x] [] = [x]
    addCoefs [] [x] = [x]
    addCoefs [x] [y] = [x + y]
    addCoefs (x:xs) [] = x : addCoefs xs []
    addCoefs [] (y:ys) = y : addCoefs ys []
    addCoefs (x:xs) (y:ys) = (x + y) : addCoefs xs ys

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P p1) (P p2) = sum (multiplyTerms p1 p2 [])
    where
    multiplyTerms [] [] _ = [P []]
    multiplyTerms [x] terms offset = [P (offset ++ map (*x) terms)]
    multiplyTerms (x:xs) terms offset = P (offset ++ map (*x) terms) : multiplyTerms xs terms ( 0 : offset)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P terms) = P (map negate terms)
    fromInteger x = P [fromInteger x]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P terms) val = sum (evalTerms (reverse terms) val (length terms - 1))
    where
        evalTerms [] _ _ = [0]
        evalTerms [t] _ _ = [t] -- last position is constant
        evalTerms (t:ts) val pow = t * val^pow : evalTerms ts val (pow - 1)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 _ = undefined
    nderiv 1 x = deriv x
    nderiv n x = deriv ( nderiv (n-1) x )

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = undefined
    deriv (P terms) = P (deriveTerms (tail terms) 1)
        where
        deriveTerms [] _ = [0]
        deriveTerms [t] pos = [t * pos] -- last term becomes constant
        deriveTerms (t:ts) pos = (t * pos) : deriveTerms ts (pos + 1)

