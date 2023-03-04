{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Data.Monoid
import Employee
import Data.Tree
import Data.List


-- exercise1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + (empFun e))

instance Monoid GuestList where
    mempty = GL [] 0
    -- mappend (GL e1 f1) (GL e2 f2) = GL (e1 <> e2) (f1 + f2)
    mappend a (GL e _) = foldr glCons a e

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1@(GL _ f1) l2@(GL _ f2) = if f1 >= f2 then l1 else l2

lonely :: Employee -> GuestList
lonely e = glCons e mempty

-- exercise 2

-- this fold can never be empty.
-- can use rootLabel and subForest labels
-- we do the traversing, and hand the given function f  value (a) and list of (b)
-- in other words, f is a -> [b] -> b
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node v s) = f v (map (treeFold f) s)

-- example: sum
treeSum :: Tree Int -> Int
treeSum (Node v l) = v + foldr (\t acc -> acc + treeSum t) 0 l
treeSum' :: Tree Int -> Int
treeSum' t = treeFold (\val results -> val + (sum results)) t

-- example: size
treeSize :: Tree Int -> Int
treeSize (Node _ l) = 1 + foldr (\t acc -> acc + treeSize t) 0 l
treeSize' :: Tree Int -> Int
treeSize' t = treeFold (\_ results -> 1 + (length results)) t

-- example: flatten
treeFlatten :: Tree Int -> [Int]
treeFlatten (Node v l) = v : foldr (\t acc -> acc ++ treeFlatten t) [] l
treeFlatten' :: Tree Int -> [Int]
treeFlatten' t = treeFold (\val results -> val : (concat results)) t

-- exercise 3

-- boss [(lBoss, lNoBoss)]
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
-- employees with no boss are always invited
nextLevel boss [] = (lonely boss, mempty)
-- choose best from each list:
-- add boss and nullify fun for 'boss' list
-- don't add boss for 2nd list.
nextLevel boss pl = (
        -- with boss, we can only take from the no boss list.
        foldr (<>) (lonely boss) (map (\p -> snd p) pl)
    ,
        -- no boss - just take the best between the boss and no-boss subordinates
        foldr (<>) mempty (map (\p -> max (fst p) (snd p)) pl)
    )

    -- foldr (\gl acc -> (bossFunnest gl acc, noBossFunnest gl acc)) (mempty, mempty) gls
    -- where
    --     bossFunnest a b = moreFun (fst a) (fst b)
    --     noBossFunnest a b = moreFun (snd a) (snd b)

-- exercise 4

-- recurse down ->
maxFun :: Tree Employee -> GuestList
maxFun t = moreFun glBoss glNoBoss
    where
        (glBoss, glNoBoss) = treeFold nextLevel t

getNames :: [Employee] -> String
getNames emps = intercalate "\n" $ sort (map empName emps)

