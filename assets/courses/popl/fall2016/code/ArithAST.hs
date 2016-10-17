module ArithAST where

data Op    = Plus | Minus | Times | Div deriving Show
data Exp   = Const Int | Aexp Op Exp Exp deriving Show