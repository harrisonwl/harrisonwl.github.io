module Zhang_HW2 where

data Program = CurlyBraces Stmt_list 
data Stmt_list = Semicolon Stmt Stmt_list | Empty 
data Stmt = Decl Decl | Assign Assign | Read_Stmt Read_Stmt | Write_stmt Write_stmt
data Decl = Integer Var | Float Var 
data Assign = Assignment Var Expr 
data Read_Stmt = Read Var 
data Write_stmt = Write Expr 
data Expr = Term Term| PlusOp Term Term | SubOp Term Term 
data Term = INTEGERNUM Int| FLOATNUM Float| Var Var | Paren Expr
data Var = NAME String

instance Show Var where
    show (NAME s) = s
    
instance Show Term where
    show (INTEGERNUM n) = show n
    show (FLOATNUM n) = show n
    show (Var a) = show a
    show (Paren n) = "(" ++ show n ++ ")"
    
instance Show Expr where
    show (Term a) = show a
    show (PlusOp a b) = show a ++ "+" ++ show b
    show (SubOp a b) = show a ++ "-" ++ show b
    
instance Show Write_stmt where
    show (Write a ) = "write" ++ " " ++ show a

instance Show Read_Stmt where
    show (Read a ) = "read" ++ " " ++ show a

instance Show Assign where
    show (Assignment a b) = show a ++ " " ++ ":=" ++ " " ++ show b
    
instance Show Decl where
    show (Integer a) = "integer" ++ " " ++ show a
    show (Float a) = "float" ++ " " ++ show a

instance Show Stmt where
    show (Decl a) = show a
    show (Assign a) = show a
    show (Read_Stmt a) = show a
    show (Write_stmt a) = show a    

instance Show Stmt_list where
    show (Semicolon a b) = show a ++ ";" ++ "\n"++ show b
    show Empty = ""
    
instance Show Program where
    show (CurlyBraces a) = "{" ++ "\n" ++ show a ++ "}"

-- a test case    
toy = CurlyBraces (Semicolon (Decl(Integer(NAME "i"))) stmtList1)
stmtList1 = Semicolon (Decl(Float(NAME "x"))) stmtList2
stmtList2 = Semicolon (Read_Stmt(Read(NAME "x"))) stmtList3
stmtList3 = Semicolon (Assign(Assignment(NAME "x")(PlusOp (Var (NAME "x"))(Var (NAME "x"))))) stmtList4
stmtList4 = Semicolon (Write_stmt(Write(Term (Var (NAME "x"))))) stmtList5
stmtList5 = Semicolon (Assign(Assignment(NAME "i")(Term (INTEGERNUM 99)))) stmtList6
stmtList6 = Empty

-- result
-- *Zhang_HW2> toy
-- {
-- integer i;
-- float x;
-- read x;
-- x := x+x;
-- write x;
-- i := 99;
-- }