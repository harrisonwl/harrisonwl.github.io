module Arith4 where

import ArithAST
import ExpParser

ex1 = Aexp Plus (Const 1) (Const 2)
ex2 = Aexp Div (Const 1) (Const 0)

{-
   Alternate form of interp3 using ">>=" instead of "do".
-}
interp4 :: Exp -> Maybe Int
interp4 (Const v)          = Just v
interp4 (Aexp Plus e1 e2)  = interp4 e1 >>= \ v1 ->
                             interp4 e2 >>= \ v2 ->
                                Just (v1+v2)
interp4 (Aexp Minus e1 e2) = interp4 e1 >>= \ v1 ->
                             interp4 e2 >>= \ v2 ->
                                Just (v1-v2)
interp4 (Aexp Times e1 e2) = interp4 e1 >>= \ v1 ->
                             interp4 e2 >>= \ v2 ->
                                Just (v1*v2)
interp4 (Aexp Div e1 e2)   = interp4 e1 >>= \ v1 ->
                             interp4 e2 >>= \ v2 -> 
                                if v2==0 
                                   then 
                                     Nothing
                                   else 
                                     Just (v1 `div` v2)

run = interp4 . parser
