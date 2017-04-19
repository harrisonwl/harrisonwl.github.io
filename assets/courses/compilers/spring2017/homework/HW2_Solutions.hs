module HW2_Solutions where


{-
Problem 1.

There isn't really a "right" answer to this question exactly. So, for example, a straightforward
encoding of the CFG as Haskell data declarations is perfectly fine:

data Program   = Program StmtList
data StmtList  = NonEmpty Stmt StmtList | Empty
data Stmt      = StmtDecl Decl
               | StmtAssign Assign
               | StmtRead ReadStmt
               | StmtWrite WriteStmt
data Decl      = IntegerDecl String | FloatDecl String
data Assign    = Assign String Expr
data ReadStmt  = ReadStmt String
data WriteStmt = WriteStmt String
data Expr      = ExprTerm Term | Plus Term Term | Sub Term Term
data Term      = INTEGERNUM Int | FLOATNUM Float | Var String | Paren Expr
-}

--
-- This is how I would answer the question. It economizes more on declarations and is
-- more abstract than the above.
--
data Program = Program [Stmt]
data Stmt    = IntegerDecl String | FloatDecl String
             | Assign String Expr
             | ReadStmt String
             | WriteStmt String
data Expr    = Plus Expr Expr | Sub Expr Expr | INTEGERNUM Int | FLOATNUM Float | Var String

{-
The following program would be represented by 'toy' below. If I used the commented syntax above,
the result would just be more verbose (but still correct).

{
integer i;
float x;

read x;
x := x + x;
write x;
i := 99;
}

-}

toy = Program [IntegerDecl "i",
               FloatDecl "x",
               ReadStmt "x",
               Assign "x" (Plus (Var "x") (Var "x")),
               WriteStmt "x",
               Assign "i" (INTEGERNUM 99)]

{-
Problem 2.

-}

instance Show Program where
  show (Program p) = "{\n" ++ foldr (\ c cs -> show c ++ ";\n " ++ cs) "" p ++ "}"

instance Show Stmt where
  show (IntegerDecl i) = "integer " ++ i
  show (FloatDecl x)   = "float " ++ x
  show (Assign x e)    = x ++ " := " ++ show e
  show (ReadStmt x)    = "read " ++ x
  show (WriteStmt x)   = "write " ++ x

instance Show Expr where
  show (Plus e1 e2)   = show e1 ++ " + " ++ show e2
  show (Sub e1 e2)    = show e1 ++ " - " ++ show e2
  show (INTEGERNUM i) = show i
  show (FLOATNUM x)   = show x
  show (Var x)        = x

{-
Now, "show" shows that the representation of toy is correct:
ghci> toy
{
integer i;
 float x;
 read x;
 x := x + x;
 write x;
 i := 99;
 }

-}
