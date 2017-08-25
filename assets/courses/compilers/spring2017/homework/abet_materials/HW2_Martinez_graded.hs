module HW2_Martinez where

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Total = 27 points = 27 + 20 - 20 (2 days late)
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Problem 1. 27/30
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data Program   = Program Stmt_list
data Stmt_list = Lambda | Semicolon Stmt Stmt_list
data Stmt      = Decl_stmt Decl | Assign Var Expr | 
                 Read_stmt Var | Write_stmt Expr
data Decl      = IntDecl NAME | FloatDecl NAME
data Expr      = Single Term | Plus Term Term | Sub Term Term
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- This is a convoluted representation; e.g., why separate Term from Expr?
-- It works, but is unnecessarily complex. -3
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data Term      = IntTerm INTEGERNUM | FloatTerm FLOATNUM | 
                 TermVar Var | TermExpr Expr
data Var       = Var NAME 

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Problem 2. 20/20
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instance Show Program where
    show (Program sl) = "{\n" ++ show sl ++ "\n}"

instance Show Stmt_list where
    show (Semicolon s sl) = show s ++ ";\n" ++ show sl
    show Lambda = show "Lambda"
 
instance Show Stmt where
    show (Decl_stmt d)  = show d
    show (Assign v e)   = show v ++ " := " ++ show e
    show (Read_stmt v)  = "read " ++ show v
    show (Write_stmt e) = "write " ++ show e

instance Show Decl where
    show (IntDecl n)   = "integer " ++ n
    show (FloatDecl n) = "float " ++ n

instance Show Expr where
    show (Single t)   = show t
    show (Plus t1 t2) = show t1 ++ " + " ++ show t2
    show (Sub t1 t2)  = show t1 ++ " - " ++ show t2

instance Show Term where
    show (IntTerm n)   = show n
    show (FloatTerm n) = show n
    show (TermVar v)   = show v
    show (TermExpr e)  = "(" ++ show e ++ ")"

instance Show Var where
    show (Var n) = n

type INTEGERNUM = Int
type FLOATNUM   = Float
type NAME       = String

-----------------------------------
-- TESTS
infixr 5 `Semicolon` -- infix Stmt_list's constructor

{-- Given test
{
integer i;
float x;

read x;
x := x + x;
write x;
i := 99;
}
--}
test = a `Semicolon` b `Semicolon` c `Semicolon` 
    d `Semicolon` e `Semicolon` f `Semicolon` g

toy = Program test

a = Decl_stmt (IntDecl "i")
b = Decl_stmt (FloatDecl "x")
c = Read_stmt (Var "x")
d = Assign (Var "x") (Plus (TermVar (Var "x")) (TermVar (Var "x")))
e = Write_stmt (Single (TermVar (Var "x")))
f = Assign (Var "i") (Single (IntTerm 99))
g = Lambda
