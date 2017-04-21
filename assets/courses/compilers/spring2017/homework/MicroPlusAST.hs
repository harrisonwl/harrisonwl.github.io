module MicroPlusAST where

import Prelude hiding (EQ,LT,GT)
  
type Name = String

data Cmd = Assign Name Exp
         | While Exp Cmd
         | Seq [Cmd]
         | IfThen Exp Cmd
         | IfThenElse Exp Cmd Cmd
         | Return Exp
         | Skip

data Exp = Var Name
         | UnaryOp Op Exp
         | BinOp Op Exp Exp
         | FunCall Name [Exp]
         | ICon Integer
         | FCon Double
         | BCon Bool

data Op = Plus | Times | Sub | Div | Neg -- arithmetic
        | GT | LT | EQ | Not | And | Or  -- boolean

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
  show (Seq cs)             =  foldr showem "" cs
                                  where showem c [] = show c
                                        showem c cs = show c ++ " ; " ++ cs
  show (IfThen e c)         = "if " ++ show e ++ " { " ++ show c ++ " }"
  show (IfThenElse e c1 c2) = "if " ++ show e ++ " { " ++ show c1 ++ " }"
                                                  ++ " else { " ++ show c2 ++ " }"
  show (Return e)           = "return " ++ show e
  show Skip                 = "skip"

parentheses s = "(" ++ s ++ ")"

instance Show Exp where
  show (Var x)          = x
  show (UnaryOp op e)     = show op ++ parentheses (show e)
  show (BinOp op e1 e2) = parentheses $ show e1 ++ " " ++ show op ++ " " ++ show e2
  show (FunCall f args) = f ++ (parentheses $ showseq "," args)
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
  show (FunDecl rt fun args [] body) = 
       show rt ++ " " ++ fun ++ "(" ++ showseq "," args ++ ")" 
         ++ " { " ++ show body ++ " }"
  show (FunDecl rt fun args locals body) = 
       show rt ++ " " ++ fun ++ "(" ++ showseq "," args ++ ")" 
         ++ " { [" ++ showseq ";" locals ++ "]" ++ " ; " 
         ++ show body ++ " }"
         
instance Show Program where
  show (Program fds) = foldr (\ f fs -> show f ++ "\n" ++ fs) "" fds

