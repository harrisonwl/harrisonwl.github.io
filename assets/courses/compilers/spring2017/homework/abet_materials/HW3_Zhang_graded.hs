
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Total = 25 + 20 + 25 + 10 = 80/100.
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
module HW3_Zhang_graded where

import Prelude hiding (EQ,LT,GT)
import Data.List

--
-- CS 4430/7430 Homework 3. Static Checks Homework.
-- Four Problems, each worth 25 points.
--
--
-- Directions:
--
-- To turn in your solution, please email me the code (harrisonwl@missouri.edu) with the subject
-- "CS4430 HW3". It is important that you get this small detail right because otherwise I may miss
-- your submitted solution. Your solution should be in the form of a single Haskell file named
-- "Last-name_HW3.hs". So, if I did this homework, I’d turn in "Harrison_HW3.hs".
--
-- Please enter all the solutions to each problem here in this file. The file you email me must
-- load into GHCi in order to receive credit.
--
-- The problems are below. Please read through the following abstract syntax which we will also
-- discuss in class. You may define any helper functions you think are necessary.
--

type Name = String

data Cmd = Assign Name Exp
         | While Exp Cmd
         | Seq Cmd Cmd
         | IfThen Exp Cmd
         | IfThenElse Exp Cmd Cmd
         | Return Exp
         | Skip

data Exp = Var Name
         | UnaryOp Op Exp
         | BinOp Op Exp Exp
         | FunCall Name [Exp]
         | ICon Int
         | FCon Float
         | BCon Bool

data Op = Plus | Times | Sub | Neg -- arithmetic
        | GT | LT | EQ | Not       -- boolean

data Type = FLOAT | INT | BOOL

data Decl = Decl Name Type

data FunDecl = FunDecl Type Name [Decl] [Decl] Cmd

newtype Program = Program [FunDecl]

instance Show Op where
  show Plus  = "+"
  show Times = "*"
  show Sub   = "-"
  show Neg   = "-"
  show GT    = ">"
  show LT    = "<"
  show EQ    = "=="
  show Not   = "not"

instance Show Cmd where
  show (Assign x e)         = x ++ " := " ++ show e
  show (While e c)          = "while " ++ show e ++ "{ " ++ show c ++ "}"
  show (Seq c1 c2)          = show c1 ++ " ; " ++ show c2
  show (IfThen e c)         = "if " ++ show e ++ " { " ++ show c ++ " }"
  show (IfThenElse e c1 c2) = "if " ++ show e ++ " { " ++ show c1 ++ " }"
                                                  ++ " else { " ++ show c2 ++ " }"
  show (Return e)           = "return " ++ show e
  show Skip                 = "skip"

parens s = "(" ++ s ++ ")"

instance Show Exp where
  show (Var x)          = x
  show (UnaryOp op e)     = show op ++ parens (show e)
  show (BinOp op e1 e2) = parens $ show e1 ++ " " ++ show op ++ " " ++ show e2
  show (FunCall f args) = f ++ (parens $ showseq "," args)
  show (ICon i)         = show i
  show (FCon x)         = show x
  show (BCon b)         = show b

instance Show Decl where
  show (Decl x t) = x ++ " : " ++ show t

showseq sep = foldr f ""
  where f d [] = show d
        f d ds = show d ++ sep ++ ds

instance Show Type where
  show FLOAT = "float"
  show INT   = "int"
  show BOOL  = "bool"

instance Show FunDecl where
  show (FunDecl rt fun args locals body) =
       show rt ++ " " ++ fun ++ "(" ++ showseq "," args ++ ")"
         ++ " { " ++ showseq ";" locals
         ++ show body ++ " }"

instance Show Program where
  show (Program fds) = foldr (\ f fs -> show f ++ "\n" ++ fs) "" fds

--
-- Here are some example programs. I will provide more shortly.


ex1 = Program fds
  where
    fds = [fac,main]
    fac = FunDecl INT "fac" [Decl "i" INT] []
             (IfThenElse
                (BinOp EQ (Var "i") (ICon 0))
                (Return (ICon 1))
                (Return (BinOp Times (Var "i") (FunCall "fac" [BinOp Sub (Var "i") (ICon 1)]))))
    main = FunDecl INT "main" [] [] (Return (FunCall "fac" [ICon 5]))


{-
Note that showing ex1 reveals that it just calculates 5!:

  ghci> putStrLn $ show ex1
  int fac(i : int) { if (i == 0) { return 1 } else { return (i * fac((i - 1))) } }
  int main() { return fac(5) }

-}

--
-- Note that "x" is undefined in ex2. Also, the true branch of the IfThenElse does not return.
--
ex2 = Program fds
  where
    fds = [fac,main]
    fac = FunDecl INT "fac" [Decl "i" INT] []
             (IfThenElse
                (BinOp EQ (Var "i") (ICon 0))
                (Assign "x" (ICon 1))
                (Return (BinOp Times (Var "i") (FunCall "fac" [BinOp Sub (Var "i") (ICon 1)]))))
    main = FunDecl INT "main" [] [] (Return (FunCall "fac" [ICon 5]))


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Problem 1. 25/25.
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    

-- Problem 1. Write a function called "uniquemain" that checks that a program p has
--  precisely one function called "main". In particular, the behavior of uniquemain
--  is given below:
--
--    uniquemain p == Left p, if program p has one and only one function called "main"
--    uniquemain p == Right "No main procedure", if program p has no "main" procedure
--    uniquemain p == Right "Too many main procedures", if program p has more than one "main".

type ErrorMessage = String

--
-- The return type of uniquemain is written with Either; recall the Prelude definition of Either is:
--
--    data Either a b = Left a | Right b
--
-- We give uniquemain that return type so that it can return different error messages.
--
uniquemain :: Program -> Either Program ErrorMessage
ismain :: FunDecl -> Bool
ismain (FunDecl rt fun args locals body) = case fun of
                                         "main" -> True
                                         _ -> False

uniquemain (Program fds) = case length (filter ismain fds) of
                         0 -> Right "No main procedure"
                         1 -> Left (Program fds)
                         _ -> Right "Too many main procedures"



--
-- Hint: recall the Prelude function, filter :: (a -> Bool) -> [a] -> [a]
-- With filter, you can create a list of all function declarations of main within a program p.
-- Then, you can inspect the length of that list.
--


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Problem 2. 20/25. This one is a really simple problem that
-- people seemed to over-think.
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    


-- Problem 2. Write a function called "symboltable" that computes a symbol table for a program.
--
-- Here, we will define a symbol table as the following:

type SymbolTable = [(Type, Name, [Decl], [Decl])]

-- Each entry in the symbol table, (t,f,params,locals), consists of a
--   * function declarations return type t,
--   * the function name f,
--   * params, and
--   * locals, which includes all declarations from the function parameters and
--     the local definitions.
-- Note that order matters in params and locals.

fundecl :: FunDecl -> (Type, Name, [Decl], [Decl])
fundecl (FunDecl rt fun args locals body) = (rt, fun, args, locals)
--                                                          ^^^^^^
-- "locals, which includes all declarations from the function parameters and
-- the local definitions." Your locals doesn't. -5

symboltable :: Program -> SymbolTable
symboltable (Program []) = []
symboltable (Program (x:xs)) = (fundecl x) : symboltable (Program xs)


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Problem 3. 25/25. Good.
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    


-- Problem 3. Define a function, noundefined :: Program -> Bool, such that
--    noundefined (Program fds) == True if, and only if, there are no undefined program variables
-- in the bodies of the function declarations fds. Here, we say that a name is a "program variable"
-- if it is a function parameter or a locally defined variable. In other words, it is not a function
-- name.
--
-- The basic approach you will take to this problem is to check that the set of all program variables
-- *used* in a function body (which is a Cmd) is a subset of the set of all program variabled
-- *defined* in a function declaration.

--
-- Here is a helper function you may use.
--
subseteq :: Eq a => ([a],[a]) -> Bool
subseteq (x, y) = x `intersect` y == x

-- Here's how subseteq works:
--  ghci> subseteq [1,2,3] [1,2,3,4,5]
--   True
--  ghci> subseteq [2,1,2,3] [1,2,3,4,5]
--   True
--  ghci> subseteq [2,1,3] [1,2,3,4,5]
--   True
--  ghci> subseteq [6] [1,2,3,4,5]
--   False

-- Here is the function you must define:
noundefined :: Program -> Bool
noundefined (Program []) = True
noundefined (Program (x:xs)) = subseteq (usedvars x, defvars x) && noundefined (Program xs)

--
-- I would suggest that you define the following helper functions.
--

decl :: Decl -> Name
decl (Decl a b) = a

-- This computes the names of all program variables defined in a FunDecl:
defvars :: FunDecl -> [Name]
defvars (FunDecl _ _ [] [] _) = []
defvars (FunDecl a b (p:ps) [] c) = [decl p] ++ defvars (FunDecl a b ps [] c)
defvars (FunDecl a b [] (l:ls) c) = [decl l] ++ defvars (FunDecl a b [] ls c)
defvars (FunDecl a b (p:ps) (l:ls) c) = [decl p] ++ [decl l] ++ defvars (FunDecl a b ps ls c)

-- This computes the names of all program variables in a FunDecl:
usedvars :: FunDecl -> [Name]
usedvars (FunDecl _ _ _ _ b) = varsCmd b

-- This computes all the names used in a Cmd:
varsCmd :: Cmd -> [Name]
varsCmd (Assign a b) = a:varsExp b
varsCmd (While a b) = varsExp a ++ varsCmd b
varsCmd (Seq a b) = varsCmd a ++ varsCmd b
varsCmd (IfThen a b) = varsExp a ++ varsCmd b
varsCmd (IfThenElse a b c) = varsExp a ++ varsCmd b ++ varsCmd c
varsCmd (Return a) = varsExp a
varsCmd _ = []

-- This computes all the names used in an Exp. Note that it is interdefined with varsCmd:
varsExp :: Exp -> [Name]
varsExp (Var a) = [a]
varsExp (UnaryOp a b) = varsExp b
varsExp (BinOp a b c) = varsExp b ++ varsExp c
varsExp (FunCall a []) = []
varsExp (FunCall a (x:xs)) = (varsExp x) ++ varsExp (FunCall a xs)
varsExp _ = []




-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--
-- Problem 4. 10/25. This problem was subtle and no one seems to have
-- gotten the point of analyzing the program "backwards" that I described
-- in class. Check the solutions.
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Problem 4. Write a function called "alwaysReturn" that checks whether a program
--  precisely one function called "main". In particular, the behavior of uniquemain
--  is given below:
--

alwaysReturn :: Program -> Either Program ErrorMessage
alwaysReturn (Program fds)
    | length (filter ismain fds) == 0 = Right "No main procedure"
    | length (filter ismain fds) > 1  = Right "Too many main procedures"
    | null cs = Left (Program fds)
    | otherwise = Right ("Following commands don't return:" ++ show cs)
    where cs = filter noreturn (map getcmd fds)

getcmd :: FunDecl -> Cmd
getcmd (FunDecl rt fun args locals body) = body


-- let bodies = map getcmd fds

-- let cs = filter noreturn bodies

-- if null cs then Left Program fds else Right ("Following commands don't return:" ++ show cs)


-- a. Create a list, bodies :: [Cmd], with all the bodies in the function declarations fds.
-- b. Write a function, noreturn :: Cmd -> Bool, so that
--      noreturn c == True if, and only if, c definitely returns.
-- c. Use filter, noreturn, and bodies to compute the list of all function bodies that definitely do no
--    return.
-- d. If the aforementioned list cs is empty, then alwaysReturn p == Left p; if cs is non-empty, then
--       alwaysReturn p == Right ("Following commands don't return:" ++ show cs)

-- Think carefully about noreturn, it should only return True if its argument *definitely* returns.

noreturn :: Cmd -> Bool
noreturn (While a b) = noreturn b
noreturn (Seq a b) = noreturn a && noreturn b
noreturn (IfThenElse a b c) = noreturn b || noreturn c
noreturn (Return _) = False
noreturn _ = True
