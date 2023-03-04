{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

-- Exercise 1

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

-- implement a Functor instance for Parser
-- fmap is (a -> b) -> f a -> f b
-- match on output of f xs : if Nothing, then return nothing,
-- else apply f to first value of the Just
instance Functor Parser where
  fmap f p = Parser cf
    where cf xs =
              case (runParser p xs) of
                  Nothing   -> Nothing
                  Just (a, b) -> Just (first f (a, b))

-- Exercise 2

-- implement Applicative for Parser
-- pure :: a -> f a.
-- pure: a Parser that just returns the parameter without consuming input
-- <*> :: f (a -> b) -> f a -> f b
-- <*>: extract function from p1, 'run' against p2

instance Applicative Parser where
  pure x = Parser (\xs -> Just (x, xs))
  p1 <*> p2 = Parser cf
    where cf xs = case (runParser p1 xs) of
                    Nothing -> Nothing
                    Just (f, rest) -> runParser (fmap f p2) rest

-- Exercise 3
-- the char parsers are type char -> Parser char
-- nothing useful is done with the first value, just discarded and replaced with the second
-- so we need to insert a function that creates a tuple into the Parser
abParser :: Parser (Char, Char)
abParser = makePair <$> char 'a' <*> char 'b'
  where makePair a b = (a,b)

-- is there a better way to convert output to ()? Is pure useful?
abParser_ :: Parser ()
abParser_ = (\a b -> ()) <$> char 'a' <*> char 'b'

-- split input before parsing, or can we discard input
intPair :: Parser [Integer]
intPair = gatherInts <$> posInt <*> char ' ' <*> posInt
  where gatherInts a spc b = [a,b]


-- Exercise 4
-- empty represents failure (so a Parser that always returns Nothing)
-- f1 <|> f2 is try f1, then try f2 if f1 fails
instance Alternative Parser where
  empty = Parser (const Nothing)
  a1 <|> a2 = Parser cf -- Parser $ liftA2 (<|>) a1 a2
   where cf xs = runParser a1 xs <|> runParser a2 xs
                      -- Nothing -> runParser a2 xs
                      -- Just x -> Just x


-- Exercise 5
intOrUppercase :: Parser ()
intOrUppercase = ((\_ -> ()) <$> posInt) <|> ((\_ -> ()) <$> satisfy isUpper)
