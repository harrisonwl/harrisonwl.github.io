module ExpParser where

import Data.Char
import Parsing
import ArithAST

parseOp :: Parser Op
parseOp = do
            isym <- symbol "+" +++ symbol "-" +++ symbol "*" +++ symbol "/"
            return (tr isym)
               where 
                  tr "+" = Plus
                  tr "-" = Minus
                  tr "*" = Times
                  tr "/" = Div

parseConst :: Parser Exp
parseConst = do 
               i <- integer
               return (Const i)

parseAexp :: Parser Exp
parseAexp = do
              symbol "("
              op <- parseOp
              space
              e1 <- parseExp
              space
              e2 <- parseExp
              symbol ")"
              return (Aexp op e1 e2)
       
parseExp :: Parser Exp
parseExp = parseConst +++ parseAexp      

--
-- This is the thing we're trying to define.
--
parser :: String -> Exp
parser inp = case parse parseExp inp of
                  [(e,_)] -> e
                  _       -> error "syntax error"

--
-- data Parser a =  P (String -> [(a,String)])
--
