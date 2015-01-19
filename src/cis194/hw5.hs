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


