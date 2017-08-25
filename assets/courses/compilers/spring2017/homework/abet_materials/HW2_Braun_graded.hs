-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Total = 27 + 15 -1 = 41/50.
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Please don't forget the module declaration. -1.
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
module HW2_Braun where


import Data.String
import Data.Char

data Var = Var String

data Term = TermInt Int
          | TermFloat Float
          | TermVar Var
          | TermExpr Expr
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Problem 1. 27/30
-- This is supposed to be an abstract syntax, so why do you
-- distinguish Term from Expr? Needlessly complex. -3.
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data Expr = Expr Term 
          | ExprPlus Term Term
          | ExprMinus Term Term

data WriteStmt = WriteStmt Expr

data ReadStmt = ReadStmt Var

data Assign = Assign Var Expr

data Decl = DeclInt String 
          | DeclFloat String
          
data Stmt = StmtDecl Decl
          | StmtAssign Assign
          | StmtRead ReadStmt
          | StmtWrite WriteStmt
          
data StmtList = StmtList Stmt StmtList
              | Nothing
              
data Program = Program StmtList

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Problem 2. 15/20
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instance Show Expr where
  show(Expr x) = show x
  show(ExprPlus x y)= show x ++ "+" ++ show y
  show(ExprMinus x y) = show x ++ "-" ++ show y
  
instance Show Var where
  show(Var x) = show x
  
instance Show Term where
  show(TermInt x) = show x
  show(TermFloat x) = show x
  show(TermExpr x) = show x
  show(TermVar x) = show x
  
instance Show WriteStmt where
  show(WriteStmt x) = "write" ++ show x
  
instance Show ReadStmt where
  show(ReadStmt x) = "read" ++ show x

instance Show Assign where
  show(Assign x y) = show x ++ ":=" ++ show y
  
instance Show Decl where
  show(DeclInt x) = "integer" ++ show x
  show(DeclFloat x) = "float" ++ show x
  
instance Show Stmt where
  show(StmtDecl x) = show x
  show(StmtAssign x) = show x
  show(StmtRead x) = show x
  show(StmtWrite x) = show x
 
instance Show StmtList where
  show(StmtList x y) = show x ++ ";" ++ show y

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- You should have added the following case. -5
-- If you had tested this, it wouldn't have worked.
--  
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instance Show Program where
  show (Program x) = "{" ++ show x ++ "}"

