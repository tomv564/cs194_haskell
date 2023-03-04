import Calc
import ExprT

main :: IO ()
main =  do
    putStrLn $ show $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    putStrLn $ show $ evalStr "2+3*4"
    putStrLn $ show $ (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT)