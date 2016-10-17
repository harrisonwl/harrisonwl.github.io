module Arith3 where

import ArithAST
import ExpParser

ex1 = Aexp Plus (Const 1) (Const 2)
ex2 = Aexp Div (Const 1) (Const 0)

interp3 :: Exp -> Maybe Int
interp3 (Const v)          = Just v
interp3 (Aexp Plus e1 e2)  = do v1 <- interp3 e1
                                v2 <- interp3 e2
                                Just (v1+v2)
interp3 (Aexp Minus e1 e2) = do v1 <- interp3 e1
                                v2 <- interp3 e2
                                Just (v1-v2)
interp3 (Aexp Times e1 e2) = do v1 <- interp3 e1
                                v2 <- interp3 e2
                                Just (v1*v2)
interp3 (Aexp Div e1 e2)   = do v1 <- interp3 e1
                                v2 <- interp3 e2
                                if v2==0 
                                   then 
                                     Nothing
                                   else 
                                     Just (v1 `div` v2)

run = interp3 . parser
