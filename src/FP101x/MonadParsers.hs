
module MonadParsers where

{-import Control.Applicative-}
import Control.Monad
import Data.Char(isDigit, digitToInt)


---------------------------------------------------------
-- Monad Structure --------------------------------------
---------------------------------------------------------

{-import Control.Monad-}

{-f :: Monad m => m a -> m b -> m (a,b)-}
{-f xs ys = do x <- xs-}
             {-y <- ys-}
             {-return (x,y)-}

{-f' :: Monad m => m a -> m b -> m (a,b)-}
{-f' xs ys = xs >>= (\x ->-}
           {-ys >>= (\y ->-}
           {-return (x,y)))-}

{-f'' :: [a] -> [b] -> [(a,b)]-}
{-f'' xs ys = xs >>= (\x ->-}
           {-ys >>= (\y ->-}
           {-return (x,y)))-}


---------------------------------------------------------
-- FP101x -----------------------------------------------
---------------------------------------------------------

{-data Tree a = Node (Tree a) (Tree a)-}
            {-| Leaf a-}

-- TODO --try rewriting all of this using Maybe in place of empty lists

newtype Parser a = P (String -> [(a, String)])

-- Functor and Applicative dummy implementations from http://stackoverflow.com/questions/31652475/defining-a-new-monad-in-haskell-raises-no-instance-for-applicative
instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  -- return :: Monad m => a -> m a
  return v = P (\inp -> [(v,inp)])
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  {-p >>= f  = P (\s -> case parse p s of [(v,s')]  -> parse (f v) s'-}
                                        {-[]        -> return [] s-}
                                        {-_         -> undefined)-}
  p >>= f = P (\s -> concat [parse (f v) s' | (v,s') <- parse p s]) -- from paper http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf

item :: Parser Char
item = P (\inp -> case inp of
                  []     -> []
                  (x:xs) -> [(x,xs)])
{-item inp = case inp of-}
             {-[]     -> []-}
             {-(x:xs) -> [(x,xs)]-}

failure :: Parser a
{-failure = P (\_ -> [])-}
failure = P (const [])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P( \inp -> case parse p inp of
                        []        -> parse q inp
                        [(v,out)] -> [(v,out)]
                        _         -> undefined)

parse :: Parser a -> String -> [(a,String)]
{-parse p inp = p inp-}
parse (P p) = p

parser :: Parser (Char,Char)
parser = do x <- item
            _ <- item
            y <- item
            return (x,y)
{-parser = item >>= \c1 ->-}
         {-item >>= \_ ->-}
         {-item >>= \c2 ->-}
         {-return (c1,c2)-}

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x
              then return x
              else failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v  <- p
             vs <- many p
             return (v:vs)

string :: String -> Parser String
string [] = return []
string (x:xs) = do r  <- char x
                   rs <- string xs
                   return (r:rs)

-- Uses String input like "[1,2,3]"
oneOrMoreDigits :: Parser String
oneOrMoreDigits = do _  <- char '['
                     d  <- digit
                     ds <- many (do _ <- char ','
                                    digit)
                     _  <- char ']'
                     return (d:ds)

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
{-p `sepby1` sep = do a <- p-}
                    {-as <- many (do _ <- sep-}
                                   {-p)-}
                    {-return (a:as)-}
p `sepby1` sep = do a <- many p
                    as <- many (do _ <- sep
                                   p)
                    return (a ++ as)

{-
expr   ::= term addop expr | term
term   ::= factor mulop term | factor
factor ::= digit | ( expr )
digit  ::= 0 | 1 | ... | 9
addop  ::= + | -
mulop  ::= * | /
-}

expr :: Parser Int
expr = do t <- term
          do _ <- char '+'
             e <- expr
             return (t + e)
           +++ return t

term :: Parser Int
term = do f <- factor
          do _ <- char '*'
             t <- term
             return (f * t)
           +++ return f

factor :: Parser Int
factor = do d <- digit
            return (digitToInt d)
         +++ do _ <- char '('
                e <- expr
                _ <- char ')'
                return e

eval :: String -> Int
eval xs = fst . head $ parse expr xs
