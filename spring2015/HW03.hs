module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

main :: IO ()
main = do
  -- print (evalSimple empty (DAssign "A" (Val 10)) "A") -- DAssign works
  -- print (run empty (Incr "A") "A") -- Incr works.
  -- print (run empty (If (Val 1) (Assign "A" (Val 555)) (Assign "A" (Val 2))) "A") -- If and DIf
  -- print (run empty (Sequence (Incr "A") (Incr "A") ) "A") -- DSequence and Sequence, works
  -- print (run (extend empty "A" 1) (While (Op (Val 4) Gt (Var "A")) (Sequence (Incr "A") (Incr "B"))) "B") -- While seems to work.
  -- print (run empty (For (Assign "A" (Val 0))
  --                       (Op (Var "A") Gt (Val 4))
  --                       (Incr "A")
  --                       (Incr "B"))
  --       "A")
  --print (run empty (Sequence (Assign "A" (Val 5)) (Assign "B" (Val 6))) "A")
  print "factorial 3"
  print (run (extend empty "In" 0) factorial "Out")
  print (run (extend empty "In" 1) factorial "Out")
  print (run (extend empty "In" 2) factorial "Out")
  print (run (extend empty "In" 3) factorial "Out")

  print "squareRoot 25"
  print (run (extend empty "A" 25) squareRoot "B")

  print "fibonacci 4"
  print (run (extend empty "In" 4) fibonacci "Out")

  -- print (run (extend empty "In" 1) factorial "Out")

  -- print (empty "A")
  -- print (extend empty "A" 5 "A")
  -- print (evalE empty (Val 5))
  -- print (evalE empty (Op (Val 1) Eql (Val 2)))
  -- print (desugar (Incr "A"))
  -- print (evalSimple empty (DAssign "A" (Val 10)) "A")
  -- print (run empty (Incr "A") "A")
  -- print (run empty (If (Val 1) (Assign "A" (Val 555)) (Assign "A" (Val 2))) "A")
  -- print (run empty (For (Assign "A" (Val 0)) (Op (Var "A") Eql (Val 3)) (Incr "A") (Incr "B")) "B")
  -- print (run (extend empty "In" 4) factorial "Out")


-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st name val x = if x == name then val else st x

empty :: State
empty = const 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var name) = st name
evalE _ (Val num) = num
evalE st (Op exp1 bop exp2)
  | bop == Plus = res1 + res2
  | bop == Minus = res1 - res2
  | bop == Times = res1 * res2
  | bop == Divide = quot res1 res2
  | bop == Gt = if res1 > res2 then 1 else 0
  | bop == Ge = if res1 >= res2 then 1 else 0
  | bop == Lt = if res1 < res2 then 1 else 0
  | bop == Le = if res1 <= res2 then 1 else 0
  | otherwise = if res1 == res2 then 1 else 0 -- Eql
  where res1 = evalE st exp1
        res2 = evalE st exp2

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign name expr) = DAssign name expr
desugar (Incr name) = DAssign name (Op (Var name) Plus (Val 1))
desugar (If expr stmt1 stmt2) = DIf expr (desugar stmt1) (desugar stmt2)
desugar (While expr stmt) = DWhile expr (desugar stmt)
desugar (For initst cond update inner) = DSequence (desugar initst) (DWhile cond (DSequence (desugar inner) (desugar update)))
desugar (Sequence stmt1 stmt2) = DSequence (desugar stmt1) (desugar stmt2)
desugar (Skip) = DSkip -- Skip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign name expr) = extend st name (evalE st expr)
evalSimple st (DIf expr stmt1 stmt2) = if evalE st expr == 0 then evalSimple st stmt2 else evalSimple st stmt1
evalSimple st while@(DWhile expr stmt) = if evalE st expr == 0 then st else evalSimple st (DSequence stmt while)
evalSimple st (DSequence stmt1 stmt2) = evalSimple (evalSimple st stmt1) stmt2
evalSimple st (DSkip) = st


run :: State -> Statement -> State
run st stmt = evalSimple st (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
