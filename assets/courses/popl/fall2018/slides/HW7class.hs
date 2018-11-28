module HW7 where

import SyntaxHW7
import ParserHW7

-- Homework6, CS4450/7450
-- Out: November 27th.
-- Due: December 7th by 3pm Central time.

-- You MUST adhere to the following guidelines to get full credit:

-- * Submit (via Canvas): a single file, named HW7_pawprint.hs, where pawprint is
--   your MU username. The file should contain definitions for every function listed below.

-- * Your submission must successfully load and typecheck in Haskell Platform to
--   get any points. For example, executing:
--      $ ghci HW7_pawprint.hs
--   should not produce any errors. We won't attempt to grade assignments that 
--   fail to load, meaning you'll get zero points for this homework. Zero. Points.

-- * Name all functions and data types exactly as they appear in the
--   assignment. Grading will be partly automated, so incorrectly named functions are
--   likely to be counted as undefined functions.

-- * The code you submit must be your own. Exceptions: you may (of course) use
--   the code we provide however you like, including examples from the slides and the
--   books.

-- * No late submissions---PLEASE START EARLY!

-- For each of the following questions, put your answer directly below the 
-- question. 

-- (1) You must use a text editor (e.g., vi, textpad, emacs, etc.) to prepare 
--     your solution. 
-- (2) You must write type declarations for each and every one of your Haskell
--     definitions.
-- (3) The program you turn in must be the product of your effort alone.

-- There are six questions, each worth 25 points, making this homework
-- worth 150 points total.

--
-- SPECIAL NOTE: Professor Harrison will do almost all of Problem 1 in class on Wednesday,
--               November 28th. So, this problem is almost for free.
--

-- Problem 1. Write a Haskell function that calculates all of the variables that occur free in its
--            argument.

freeProg :: Prog -> [String]
freeProg (fds,stmts) = undefined

freeExp :: [Name] -> Exp -> [Name]
freeExp seen e = case e of
    Add e1 e2    -> freeExp seen e1 ++ freeExp seen e2
    Sub e1 e2    -> freeExp seen e1 ++ freeExp seen e2
    Mul e1 e2    -> freeExp seen e1 ++ freeExp seen e2
    Neg e        -> freeExp seen e
    LitInt _     -> []
    Var x        -> if lkup x seen then [] else [x]
    FunCall f es -> f_free ++ es_free
      where
        f_free = if lkup f seen then [] else [f]
        es_free = concat (map (freeExp seen) es)
          
lkup _ []     = False
lkup x (y:ys) = x==y || lkup x ys
  
freeBool :: [Name] -> BExp -> [Name]
freeBool = undefined

freeFD :: FunDefn -> [Name]
freeFD (n,ps,body) = undefined

freeStmt :: [Name] -> Stmt -> [Name]
freeStmt seen s = case s of
      Assign x e | x `elem` seen -> freeExp seen e
                 | otherwise     -> x : freeExp seen e
      If tst sts1 sts2           -> freeBool seen tst
                                       ++ freeStmts seen sts1
                                       ++ freeStmts seen sts2
      While tst sts              -> freeBool seen tst ++ freeStmts seen sts
      Let x e sts                -> freeExp seen e ++ freeStmts (x:seen) sts
      Switch e brs               -> freeExp seen e
                                       ++ freeStmts seen allsts
                               where
                                 allsts = concat (map snd brs)
      For i e1 e2 sts            -> freeExp seen e1
                                       ++ freeExp seen e2
                                       ++ freeStmts (i:seen) sts
      Return e                   -> freeExp seen e

freeStmts :: [Name] -> [Stmt] -> [Name]
freeStmts seen []      = undefined
freeStmts seen (s:sts) = undefined

-- Problem 2. Write a Haskell function that determines if all the defined function names
--            in a given program are unique. E.g., you should not be able to define the
--            same function twice.
uniqueFuns :: Prog -> Bool
uniqueFuns = undefined

-- Problem 3. Write a Haskell function that checks that each function in the program ends with
--            a "Return". That is, literally the last statement in the body of a function must
--            be a Return.
returning :: Prog -> Bool
returning = undefined

-- Problem 4. Write a Haskell function that checks that formal parameters of a function definition
--            are unique. E.g., "function f(i,j) ..." has unique names but "function f(i,i) ..."
--            does not.
uniqueparams :: Prog -> Bool
uniqueparams = undefined

-- Problem 5. Write a Haskell function that performs "constant folding". Constant folding means
--            any Exp that does _not_ include a Var or a FunCall is replaced by its value.
--            So, for example, this program:
--    function foo(n) {
--      return (n+2)+3;
--    }
--    y := foo(5+7);
--
-- Constant folding this program would return:
--    function foo(n) {
--      return (n+2)+3;
--    }
--    y := foo(12);
--
-- Note that "(n+2)+3" is not replaced by "n+5" with constant folding. Had the program
-- had "return n+(2+3);" instead, it would have been replaced by "n+5".
cfold :: Prog -> Prog
cfold = undefined

-- Hint: try writing a helper function, cfoldExp :: Exp -> Exp, that performs constant folding
-- on its input. Make sure that this function works correctly for "(n+2)+3" and "n+(2+3)".

-- Problem 6. Write a static checker that ensures:
--            1. The program has no free variables;
--            2. Has unique function names and each function has unique parameters;
--            3. Ensures each function ends with a Return; and
--            4. If the program passes 1.-3., perform constant folding and return the result.

staticcheck :: Prog -> Maybe Prog
staticcheck = undefined

