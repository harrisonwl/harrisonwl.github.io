module Arith1 where

import ArithAST
import ExpParser

ex1 = Aexp Plus (Const 1) (Const 2)
ex2 = Aexp Div (Const 1) (Const 0)

interp :: Exp -> Int
interp (Const v)       = v
interp (Aexp Plus e1 e2)  = interp e1 + interp e2
interp (Aexp Minus e1 e2) = interp e1 - interp e2
interp (Aexp Times e1 e2) = interp e1 * interp e2
interp (Aexp Div e1 e2)   = interp e1 `div` interp e2

run :: String -> Int
run = interp . parser
