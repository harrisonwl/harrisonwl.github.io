module Arith5 where

import ArithAST
import ExpParser

ex1 = Aexp Plus (Const 1) (Const 2)
ex2 = Aexp Div (Const 1) (Const 0)

throw :: Bool -> a -> Maybe a
throw b v = if b then Nothing else return v

{-
   Alternate form of interp4 using "return/throw" instead of "Just/Nothing".
-}
interp5 :: Exp -> Maybe Int
interp5 (Const v)          = return v
interp5 (Aexp Plus e1 e2)  = interp5 e1 >>= \ v1 ->
                             interp5 e2 >>= \ v2 ->
                                return (v1+v2)
interp5 (Aexp Minus e1 e2) = interp5 e1 >>= \ v1 ->
                             interp5 e2 >>= \ v2 ->
                                return (v1-v2)
interp5 (Aexp Times e1 e2) = interp5 e1 >>= \ v1 ->
                             interp5 e2 >>= \ v2 ->
                                return (v1*v2)
interp5 (Aexp Div e1 e2)   = interp5 e1 >>= \ v1 ->
                             interp5 e2 >>= \ v2 -> 
                                throw (v2==0) (v1 `div` v2)

run = interp5 . parser

