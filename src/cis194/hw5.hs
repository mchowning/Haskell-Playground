-- {-#OPTIONS_GHC -Wall -Werror #-}

import Hw5ExprT
import Hw5Parser
import Test.HUnit

--- Exercise 1

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalTest :: Test
evalTest = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) ~?= 20


--- Exercise 2

evalStr :: String -> Maybe Integer
evalStr s = case myParse s of Nothing  -> Nothing
                              Just a   -> Just (eval a)
  where
    myParse :: String -> Maybe ExprT
    myParse = parseExp Lit Add Mul

evalStrTest :: Test
evalStrTest = TestList [ evalStr "(2*)3+4" ~?= Nothing
                       , evalStr "(2*3)+4" ~?= Just 10
                       , evalStr "2*(3+4)" ~?= Just 14
                       ]


--- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

exprClassTest :: Test
exprClassTest = (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) ~?= Mul (Add (Lit 2) (Lit 3)) (Lit 4)


--- Exercise 4


instance Expr Integer where
  lit a   = a
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit a | a > 0     = True
        | otherwise = False
  add a b = a || b
  mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit                       = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit a = Mod7 (mod a 7)
  add (Mod7 a) (Mod7 b) = Mod7 $ mod (a + b) 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a * b) 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instanceTests = TestList [ (testExp :: Maybe Integer) ~?= Just (-7)
                         , (testExp :: Maybe Bool)    ~?= Just True
                         , (testExp :: Maybe MinMax)  ~?= Just (MinMax 5)
                         , (testExp :: Maybe Mod7)    ~?= Just (Mod7 0)
                         ]
