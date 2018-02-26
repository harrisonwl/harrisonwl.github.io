
{-
HW1, CS 4430, Spring 2018.
Due Wednesday, January 31 by 11:59pm.

Directions.

Read these carefully.

There are two problems for HW1, each worth 10 points. 

What to submit: a single file, named HW1_pawprint.hs, where pawprint is
your MU username. The file should contain definitions for every function listed below.
Furthermore, everyone should adhere to the following guidelines to get full credit:

* Your submission must successfully load and typecheck in Haskell Platform to
get any points. For example, executing:
     $ ghci HW1_pawprint.hs
should not produce any errors. I won't attempt to grade assignments that fail to load.

* Name all functions and data types exactly as they appear in the assignment. 

* The code you submit must be your own. Exceptions: you may (of course) use
the code we provide however you like, including examples from the slides and the
book.

* No late submissions---PLEASE START EARLY!

-}

module HW1_Answer where

import ImpSyntax
import ImpParser hiding (expr,stmt)

-- Static checking.
-- Imp programs should not have undefined variables. That is, every variable occurring in an Imp
-- program must be within the body of a "let" statement or else it is undefined.
-- For example:
--    Good: let x := 0 in { x := x + 1; } ; let y := 99 in { y := y + 1; } ;
--    Bad:  let x := 0 in { x := x + 1; } ; x := x + 1 ;
-- In Bad, these occurrences of x           ^    ^  are undefined.

-- In this problem, you will write three "check" functions of the following types:
--    check :: [Stmt] -> [Name] -> Bool
--    checkExp :: Exp -> [Name] -> Bool
--    checkBExp :: BExp -> [Name] -> Bool

-- In a call (check (c:cs) seen), you will inspect c for undefined variables, returning False if you
-- find one and True otherwise. Cases involving variable reference and definitions are the most important.
-- For example, in (check (Let x e cs) seen), you will need to inspect e for undefined variables and then
-- recursively calling check on cs in an expanded "seen list".

-- Problem 1. Fill in the definition of check, checkExp, and checkBExp below.

check :: [Stmt] -> [Name] -> Bool
check [] _        = True
check (c:cs) seen = case c of
  Assign x e | x `elem` seen && checkExp e seen -> check cs seen
             | otherwise                        -> False
  If be cs1 cs2              -> checkBExp be seen && check cs1 seen && check cs2 seen
  While be body              -> checkBExp be seen && check body seen && check cs seen
  Let x e body               -> checkExp e seen && check body (x:seen) && check cs seen

checkExp :: Exp -> [Name] -> Bool
checkExp (Plus e1 e2) seen = checkExp e1 seen && checkExp e2 seen
checkExp (Subt e1 e2) seen = checkExp e1 seen && checkExp e2 seen
checkExp (Mult e1 e2) seen = checkExp e1 seen && checkExp e2 seen
checkExp (Negt e) seen     = checkExp e seen
checkExp (Var x) seen      = x `elem` seen
checkExp (LitInt _) _      = True

checkBExp :: BExp -> [Name] -> Bool
checkBExp (IsEq e1 e2) seen  = checkExp e1 seen && checkExp e2 seen
checkBExp (IsNEq e1 e2) seen = checkExp e1 seen && checkExp e2 seen
checkBExp (IsGT e1 e2) seen  = checkExp e1 seen && checkExp e2 seen
checkBExp (IsLT e1 e2) seen  = checkExp e1 seen && checkExp e2 seen
checkBExp (IsGTE e1 e2) seen = checkExp e1 seen && checkExp e2 seen
checkBExp (IsLTE e1 e2) seen = checkExp e1 seen && checkExp e2 seen
checkBExp (And be1 be2) seen = checkBExp be1 seen && checkBExp be2 seen
checkBExp (Or be1 be2) seen  = checkBExp be1 seen && checkBExp be2 seen
checkBExp (Not be) seen      = checkBExp be seen
checkBExp (LitBool _) seen   = True

-- Problem 2. Alter the interp function so that, if there is an undefined variable in the
-- Imp code, that the interpreter is not called. Instead of returning the (Maybe Store) as it currently
-- does, change interp to:
--    1. print out the value of "ans", if there are no undefined variables;
--    2. print out "Undefined Variable" otherwise.

-- The type of interp will change to:
--             interp :: FilePath -> IO ()
-- To print to the terminal, use the following built-in function:
--        putStrLn :: String -> IO ()
--
-- Sample output is as follows:
--    ghci> interp "succeed1.imp"
--    Answer is: 0
--    ghci> interp "fail1.imp"
--    Undefined Variable

-- There are a number of test cases, named either "succeed" or "fail" which should produce the output
-- corresponding to their name.

-- interp :: FilePath -> IO (Maybe Store)
-- interp f = do
--   cs <- parseImp f
--   return $ stmts cs sto0

interp :: FilePath -> IO ()
interp f = do
  cs <- parseImp f
  if check cs ["ans"]
    then case stmts cs sto0 of
              Just final -> putStrLn $ "Answer is: " ++ show i
                 where Just i = lookup "ans" final
              Nothing    -> putStrLn "Crash"
    else putStrLn "Undefined Variable"

type Store = [(Name,Int)]
sto0       = [("ans",0)] -- initial store

replace :: Name -> Int -> Store -> Maybe Store
replace x i []         = Nothing 
replace x i ((y,v):cs) | x == y = return $ (x,i) : cs
                       | x /= y = do
                                     cs' <- replace x i cs
                                     return $ (y,v) : cs'

stmts :: [Stmt] -> Store -> Maybe Store
stmts [] _     = Nothing -- "empty" program
stmts (c:cs) s = do
                    s' <- stmt c s
                    case cs of
                         [] -> return s'
                         _  -> stmts cs s'

stmt :: Stmt -> Store -> Maybe Store
stmt (Assign x e) s    = do
                            i <- expr e s
                            s' <- replace x i s
                            return s'
stmt (If be cs1 cs2) s = do
                            bv <- bexp be s
                            if bv then stmts cs1 s else stmts cs2 s
stmt l@(While be cs) s = do
                            bv <- bexp be s
                            if bv then stmts (cs++[l]) s else return s
stmt (Let x e cs) s    = do
                            i <- expr e s
                            s' <- stmts cs ((x,i):s)
                            pop s'
     where
       pop :: Store -> Maybe Store
       pop []           = Nothing
       pop (cell:cells) = return cells


expr :: Exp -> Store -> Maybe Int
expr (Plus e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 + i2
expr (Subt e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 - i2
expr (Mult e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 * i2
expr (Negt e) s     = do
                         i <- expr e s
                         return $ - i
expr (Var x) s      = lookup x s
expr (LitInt i) s   = return i

bexp :: BExp -> Store -> Maybe Bool
bexp (IsEq e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 == i2
bexp (IsNEq e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 /= i2
bexp (IsGT e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 > i2
bexp (IsLT e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 < i2
bexp (IsGTE e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 >= i2
bexp (IsLTE e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 <= i2
bexp (And b1 b2) s = do
                         bv1 <- bexp b1 s
                         bv2 <- bexp b2 s
                         return $ bv1 && bv2
bexp (Or b1 b2) s  = do
                         bv1 <- bexp b1 s
                         bv2 <- bexp b2 s
                         return $ bv1 || bv2
bexp (Not b) s      = do
                         tf <- bexp b s
                         return $ not tf
bexp (LitBool bv) s = return bv


