module ExpParser where

import Data.Char
import Parsing
import ArithAST

parseOp = do
            isym <- symbol "+" +++ symbol "-" +++ symbol "*" +++ symbol "/"
            return (tr isym)
               where 
                  tr "+" = Plus
                  tr "-" = Minus
                  tr "*" = Times
                  tr "/" = Div

parseConst = do 
               i <- integer
               return (Const i)

parseAexp = do
              symbol "("
              op <- parseOp
              space
              e1 <- parseExp
              space
              e2 <- parseExp
              symbol ")"
              return (Aexp op e1 e2)
       
parseExp = parseConst +++ parseAexp      
deP (P x) = x

parser inp = fst (head ((deP parseExp) inp))
