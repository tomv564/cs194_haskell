{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- run p until it returns Nothing
-- can we use a fold instead of recursion?
-- assume we avoid nothings, and when we get nothing we terminate with an empty list
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (:) <$> p <*> zeroOrMore p <|> pure []

-- parse one regularly, then go to the recursive zero or more.
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------


spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> (zeroOrMore (satisfy isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom = ( N <$> posInt ) <|> ( I <$> ident )

-- parseSingleExpr = Comb <$> (oneOrMore)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (A <$> parseAtom <|> Comb <$> parseExpressions) <* spaces
  where
    parseAtom = ( N <$> posInt ) <|> ( I <$> ident )
    parseExpressions = char '(' *> oneOrMore parseSExpr <* char ')'
