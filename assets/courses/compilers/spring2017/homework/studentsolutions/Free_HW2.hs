module Free_HW2 where

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Total = 50 points.
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Toy Abstract Syntax

--Test command:
--putStr $ show $ PROGRAM (LIST (DECL (INTEGER "i")) (LIST (DECL (FLOAT "x")) (LIST (READ_STMT (READ (VAR "x"))) (LIST (ASSIGNMENT (ASSIGN (VAR "x") (ADD (VARIABLE (VAR "x")) (VARIABLE (VAR "x"))))) (LIST (WRITE_STMT (WRITE (VAL (VARIABLE (VAR "x"))))) (LIST (ASSIGNMENT (ASSIGN (VAR "i") (VAL (INTEGERNUM 99)))) END))))))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Good. 30/30. This is more verbose than necessary; check out the solutions.
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Problem 1.
data Program    = PROGRAM Stmt_List
data Stmt_List  = LIST Stmt Stmt_List | END
data Stmt       = DECL Decl | ASSIGNMENT Assign | READ_STMT Read_Stmt | WRITE_STMT Write_Stmt
data Decl       = INTEGER String | FLOAT String
data Assign     = ASSIGN Var Expr
data Read_Stmt  = READ Var
data Write_Stmt = WRITE Expr
data Expr       = VAL Term | ADD Term Term | SUB Term Term
data Term       = INTEGERNUM Int | FLOATNUM Float | VARIABLE Var | EXPR Expr
data Var        = VAR String

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- 20/20. 
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Problem 2.
instance Show Program where
  show (PROGRAM x) = "{\n" ++ show x ++ "}\n"

instance Show Stmt_List where
  show (LIST x y) = show x ++ ";\n" ++ show y
  show END        = ""

instance Show Stmt where
  show (DECL x)       = show x
  show (ASSIGNMENT x) = show x
  show (READ_STMT x)  = show x
  show (WRITE_STMT x) = show x

instance Show Decl where
  show (INTEGER x) = "integer " ++ id x
  show (FLOAT x)   = "float " ++ id x

instance Show Assign where
  show (ASSIGN x y) = show x ++ " := " ++ show y

instance Show Read_Stmt where
  show (READ x) = "read " ++ show x

instance Show Write_Stmt where
  show (WRITE x) = "write " ++ show x

instance Show Expr where
  show (VAL x)   = show x
  show (ADD x y) = show x ++ " + " ++ show y
  show (SUB x y) = show x ++ " - " ++ show y

instance Show Term where
  show (INTEGERNUM x) = show x
  show (FLOATNUM x)   = show x
  show (VARIABLE x)   = show x
  show (EXPR x)       = "( " ++ show x ++ " )"

instance Show Var where
  show (VAR x) = id x
