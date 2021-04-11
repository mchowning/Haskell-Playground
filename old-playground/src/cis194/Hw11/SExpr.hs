{- CIS 194 HW 11
   due Monday, 8 April
-}

{-#OPTIONS_GHC -Wall -Werror #-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

import Test.HUnit

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = ((:) <$> p <*> zeroOrMore p) <|> empty

ex1Tests :: IO Counts
ex1Tests = runTestTT $ TestList
  [ runParser (zeroOrMore $ char 'p') "ppx" ~?= Just ("pp","x")
  , runParser (oneOrMore $ char 'p') "ppx"  ~?= Just ("pp","x")
  , runParser (zeroOrMore $ char 'p') "x"   ~?= Just ("","x")
  , runParser (oneOrMore $ char 'p') "x"    ~?= Nothing ]

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ char ' '

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

ex2Tests :: IO Counts
ex2Tests = runTestTT $ TestList
  [ runParser spaces "  x" ~?= Just ("  ","x")
  , runParser spaces "la"  ~?= Just ("", "la")

  , runParser ident "foobar baz" ~?= Just ("foobar"," baz")
  , runParser ident "foo33fA"    ~?= Just ("foo33fA","")
  , runParser ident "2bad"       ~?= Nothing
  , runParser ident ""           ~?= Nothing
  ]

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show, Eq)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Show, Eq)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> parseCombo) <* spaces

parseCombo :: Parser SExpr
parseCombo = Comb <$> (char '(' *> oneOrMore parseSExpr <* char ')')

parseAtom :: Parser SExpr
parseAtom = A <$> parseAtom'
  where
    parseAtom' :: Parser Atom
    parseAtom' = (N <$> posInt) <|> (I <$> ident)


ex3Tests :: IO Counts
ex3Tests = runTestTT $ TestList
  [ TestLabel "parseAtom" $ TestList
    [ runParser parseAtom "5" ~?= Just (A (N 5), "")
    , runParser parseAtom "foo33fA" ~?= Just (A (I "foo33fA"), "")
    , runParser parseAtom "foo33fA bar" ~?= Just (A (I "foo33fA"), " bar")
    , runParser parseAtom "3f" ~?= Just (A (N 3), "f")
    ]

  , TestLabel "parseSExpr" $ TestList
    [ runParser parseSExpr "5" ~?= Just (A (N 5), "")
    , runParser parseSExpr "foo3" ~?= Just (A (I "foo3"), "")
    , runParser parseSExpr "(5 foo3)" ~?= Just (Comb [A (N 5), A (I "foo3")], "")
    , runParser parseSExpr "(bar (foo) 3 5 874)" ~?= Just (Comb [A (I "bar"),Comb [A (I "foo")],A (N 3),A (N 5),A (N 874)],"")
    , runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" ~?= Just (Comb [Comb [Comb [A (I "lambda"),A (I "x"),Comb [A (I "lambda"),A (I "y"),Comb [A (I "plus"),A (I "x"),A (I "y")]]],A (N 3)],A (N 5)],"")
    , runParser parseSExpr "( lots of ( spaces in ) this ( one ) )" ~?= Just (Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]],"")
    , runParser parseSExpr "2bad" ~?= Nothing ]
  ]
