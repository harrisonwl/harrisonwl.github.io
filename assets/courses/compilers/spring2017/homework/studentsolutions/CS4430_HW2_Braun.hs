import Data.String
import Data.Char

data Var = Var String

data Term = TermInt Int
          | TermFloat Float
          | TermVar Var
          | TermExpr Expr
  
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
  
instance Show Program where
  show(Program x) = "{" ++ show x ++ "}"

