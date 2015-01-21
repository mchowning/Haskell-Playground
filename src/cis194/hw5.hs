{-#OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import qualified Hw5ExprT as H
import qualified Data.Map as M
import Data.Functor
import Control.Applicative
import Hw5Parser
import Hw5StackVM
import Test.HUnit


--- Exercise 1

eval :: H.ExprT -> Integer
eval (H.Lit n) = n
eval (H.Add e1 e2) = eval e1 + eval e2
eval (H.Mul e1 e2) = eval e1 * eval e2

evalTest :: Test
evalTest = eval (H.Mul (H.Add (H.Lit 2) (H.Lit 3)) (H.Lit 4)) ~?= 20


--- Exercise 2

evalStr :: String -> Maybe Integer
evalStr s = case myParse s of Nothing  -> Nothing
                              Just a   -> Just (eval a)
  where
    myParse :: String -> Maybe H.ExprT
    myParse = parseExp H.Lit H.Add H.Mul

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

instance Expr H.ExprT where
  lit = H.Lit
  add = H.Add
  mul = H.Mul

exprClassTest :: Test
exprClassTest = (mul (add (lit 2) (lit 3)) (lit 4) :: H.ExprT) ~?= H.Mul (H.Add (H.Lit 2) (H.Lit 3)) (H.Lit 4)


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

instanceTests :: Test
instanceTests = TestList [ (testExp :: Maybe Integer) ~?= Just (-7)
                         , (testExp :: Maybe Bool)    ~?= Just True
                         , (testExp :: Maybe MinMax)  ~?= Just (MinMax 5)
                         , (testExp :: Maybe Mod7)    ~?= Just (Mod7 0)
                         ]


--- Exercise 5

instance Expr Program where
  lit a = [PushI a]
  add a b = a ++ b ++ [Add]
  mul a b = a ++ b ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul


--- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = Vlit Integer
              | Vadd VarExprT VarExprT
              | Vmul VarExprT VarExprT
              | Vvar String
  deriving (Show)

-- instance Show VarExprT where
--   show (Vlit a) = show a

instance Expr VarExprT where
  lit = Vlit
  add = Vadd
  mul =  Vmul

instance HasVars VarExprT where
  var = Vvar

instance HasVars (M.Map String Integer -> Maybe Integer) where
   var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add e1 e2 m = (+) <$> e1 m <*> e2 m
  mul e1 e2 m = (*) <$> e1 m <*> e2 m


withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs


