module CompilerMicroPlus where

import Prelude hiding (EQ,LT,GT)
import MicroPlusAST
import MicroPlusParser
import Data.List

type ErrorMessage = String

--
-- Checking for unique main function declaration.
--

uniquemain :: Program -> Either Program ErrorMessage
uniquemain p@(Program fds) | no_mains==1 = Left p
                           | no_mains==0 = Right "No main procedure"
                           | otherwise   = Right "Too many main procedures"
  where
    fs       = map (\ (FunDecl _ f _ _ _) -> f) fds
    no_mains = length (filter ("main"==) fs)


--
-- Symbol table creation.
--

type SymbolTable = [(Type, Name, [Decl], [Decl])]

symboltable :: Program -> SymbolTable
symboltable (Program fds) = map definedNamesFD fds

definedNamesFD :: FunDecl -> (Type, Name, [Decl], [Decl])
definedNamesFD (FunDecl t f params locals _) = (t,f,params,locals)

--
-- No undefined program variables.
--

subseteq :: Eq a => ([a],[a]) -> Bool
subseteq (x, y) = x `intersect` y == x

noundefined :: Program -> Bool
noundefined (Program fds) = foldr ((&&) . subseteq) True (zip uvs dvs)
  where
    dvs = map defvars fds
    uvs = map usedvars fds

--
-- I would suggest that you define the following helper functions.
--

-- This computes the names of all program variables defined in a FunDecl:
defvars :: FunDecl -> [Name]
defvars (FunDecl _ _ params locals _) = map (\ (Decl x t) -> x) (params ++ locals)

-- This computes the names of all program variables in a FunDecl:
usedvars :: FunDecl -> [Name]
usedvars (FunDecl _ _ _ _ b) = varsCmd b

-- This computes all the names used in a Cmd:
varsCmd :: Cmd -> [Name]
varsCmd c = case c of
  Assign x e         -> x : varsExp e
  While b c          -> varsExp b ++ varsCmd c
  IfThen b c         -> varsExp b ++ varsCmd c
  IfThenElse b c1 c2 -> varsExp b ++ varsCmd c1 ++ varsCmd c2
  Seq cs             -> concat (map varsCmd cs)
  Return e           -> varsExp e
  Skip               -> []

-- This computes all the names used in an Exp. Note that it is interdefined with varsCmd:
varsExp :: Exp -> [Name]
varsExp e = case e of
  Var x         -> [x]
  UnaryOp _ e   -> varsExp e
  BinOp _ e1 e2 -> varsExp e1 ++ varsExp e2
  FunCall _ es  -> concat $ map varsExp es
  _             -> []

---------------------------------------
-- Problem 4. Write a function called "alwaysReturn" that checks whether a program 
--  precisely one function called "main". In particular, the behavior of uniquemain
--  is given below:    
--      

alwaysReturn :: Program -> Either Program ErrorMessage
alwaysReturn p@(Program fds) = case filter noreturn bodies of
  [] -> Left p
  cs -> Right $ "Following commands don't return:" ++ show cs
  where bodies = map (\ (FunDecl _ _ _ _ b) -> b) fds

noreturn :: Cmd -> Bool
noreturn c = case c of
  Return _           -> False
  While _ c          -> noreturn c
  IfThen b c         -> True
  IfThenElse b c1 c2 -> noreturn c1 && noreturn c2
  Seq cs             -> or (map noreturn cs)
  _                  -> True
