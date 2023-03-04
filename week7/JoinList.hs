{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

jlbToList :: Monoid m => JoinList m a -> [a]
jlbToList Empty          = []
jlbToList (Single _ a)     = [a]
jlbToList (Append _ l1 l2) = jlbToList l1 ++ jlbToList l2

-- should append the joinlists while ensuring the Monoid is also updated.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
-- Empty +++ b = b
-- a +++ Empty = a
-- a +++ b = Append (mappend (tag a) (tag b)) a b
a +++ b = Append (tag a <> tag b) a b

-- a@(Single ma _) +++ b@(Single mb _) = Append (mappend ma mb) a b
-- a@(Single ma _)  +++ b@(Append mb _ _) = Append (mappend ma mb) a b
-- a@(Append ma _ _)  +++ b@(Single mb _) = Append (mappend ma mb) a b
-- a@(Append ma _ _)  +++ b@(Append mb _ _) = Append (mappend ma mb) a b

-- should get annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _ ) = m

-- val :: Monoid m => JoinList m a -> a
-- val Empty = mempty
-- val (Single _ a) = a

testList :: JoinList Size String
testList = (Append (Size 4)
                ( Append (Size 2)
                    (Single (Size 1) "trick joke")
                    (Single (Size 1) "happy dude")
                )
                ( Append (Size 2)
                    (Single (Size 1) "smile corn")
                    (Single (Size 1) "drown duck")
                )
           )

intSize :: (Sized m, Monoid m) => JoinList m a -> Int
intSize l = getSize $ size $ tag l

-- find JoinList at index, use size tag b to get num elements at the tree
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i Empty = Nothing
indexJ i (Single m a) | Size i == size m  = Just a
                      | otherwise         = Nothing
indexJ i (Append m a b) | i <= lsize = indexJ i a -- must be on the left
                        | i > lsize = indexJ (i - lsize) b -- must be on the right
                        | otherwise       = Nothing
                        where
                            lsize = intSize a

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i s@(Single m a) = if i == 0 then s else Empty
dropJ i (Append m a b) | i <= lsize = (dropJ i a) +++ b -- go left, keep right
                       | i > lsize = dropJ (i - lsize) b -- skip left, go right
                       where
                            lsize = intSize a

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i s | i == intSize s = s
          -- | otherwise      = Empty
takeJ i (Append m a b) | i <= lsize = takeJ i a
                       | i > lsize  = a +++ takeJ (i - lsize) b
                        where
                            lsize = intSize a

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

fromLine :: String -> JoinList (Score, Size) String
fromLine l = Single (scoreString l, Size 1) l

fromLines :: String -> JoinList (Score, Size) String
fromLines s = foldr (+++) Empty $ map fromLine $ lines s

instance Buffer (JoinList (Score, Size) String) where

  -- | Convert a buffer to a String.
  -- toString :: b -> String
  toString jl = unlines $ jlbToList jl

  -- | Create a buffer from a String.
  -- fromString :: String -> b
  fromString s = fromLines s

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  -- line :: Int -> b -> Maybe String
  line i jl = indexJ i jl

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  -- replaceLine :: Int -> String -> b -> b
  replaceLine n l b | n < intSize b = (takeJ n b) +++ (fromLine l) +++ (dropJ (n+1) b)
                    | otherwise      = b


  -- | Compute the number of lines in the buffer.
  -- numLines :: b -> Int
  numLines jl = intSize jl

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  -- value :: b -> Int
  value Empty = 0
  value (Single (Score s, _) _) = s
  value (Append (Score s, _) _ _) = s


