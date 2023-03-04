main :: IO ()
main = do
    putStrLn $ show $ fun1 [1,2,3,4]
    putStrLn $ show $ fun1' [1,2,3,4]
    putStrLn $ show $ fun2 5
    putStrLn $ show $ fun2' 5
    putStrLn $ show $ foldTree "ABC"
    putStrLn $ show $ xor [True, False, True, True, False, False]
    putStrLn $ show $ map' (*2) [1, 2, 3]


fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldl (*) 1 $ map (subtract 2) $ filter even xs


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = sum $ filter even (takeWhile (/= 1) (iterate (\m -> if even m then m `div` 2 else 3 * m + 1) n))

-- Hint: For this problem you may wish to use the functions iterate and takeWhile.
-- Look them up in the Prelude documentation to see what they do.
-- iterate f x returns an infinite list of repeated applications of f to x:
-- > iterate f x == [x, f x, f (f x), ...]

-- takeWhile, applied to a predicate p and a list xs, returns the longest prefix (possibly empty) of xs of elements that satisfy p:
-- > takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
-- > takeWhile (< 9) [1,2,3] == [1,2,3]
-- > takeWhile (< 0) [1,2,3] == []

data Tree a = Leaf
             | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)

-- foldTree "ABC"

foldTree :: [a] -> Tree a
foldTree xs = foldr addNode Leaf xs
    where
        addNode x Leaf = Node 0 Leaf x Leaf
        addNode x (Node height Leaf val right) = Node (height+1) (addNode x Leaf) val right
        addNode x (Node height left val Leaf) = Node height left val (addNode x Leaf)
        addNode x (Node height left@(Node lh _ _ _) val right@(Node rh _ _ _)) | lh >= rh = Node (updatedHeight height rh) left val (addNode x right)
        addNode x (Node height left@(Node lh _ _ _) val right@(Node rh _ _ _)) | rh > lh = Node height (addNode x left) val right
        updatedHeight lh rh = if lh == rh then rh + 2 else rh + 1


xor :: [Bool] -> Bool
xor xs = foldl (\ a b -> not a) False $ filter (==True) xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x a -> (f x) : a) [] xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
    let excludes = map (\(i, j) -> i + j + (2 * i * j)) $ cartProd [1..n] [1..n]
        in map makePrime $ filter (flip notElem excludes) [1..n]
  where
    makePrime x = x * 2 + 1


cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]




