module CompilerMicroPlus where

import Prelude hiding (EQ,LT,GT)
import MicroPlusAST
import MicroPlusParser
import Data.List

import FrontEndTuple
import Control.Monad.Identity
import Control.Monad.State

type ErrorMessage = String

compile file = do
  p <- file2prog file     -- lexing through parsing
  check (uniquemain p)
  check (alwaysReturn p)
  if noundefined p then return () else fail "Undefined program variables"
  let p' = mangle p
  print p'    
  let syms = mkst p'
  return (translate syms p)

mangleName f x = f ++ "_" ++ x

mangle :: Program -> Program
mangle (Program fds) = Program (map mangleFD fds)

mangleFD :: FunDecl -> FunDecl
mangleFD (FunDecl t f args locs body) = FunDecl t f args' locs' body'
   where args' = map (\ (Decl x t) -> Decl (mangleName f x) t) args
         locs' = map (\ (Decl x t) -> Decl (mangleName f x) t) locs
         body' = mangleCmd f body

mangleCmd :: Name -> Cmd -> Cmd
mangleCmd f (Assign x e)         = Assign (mangleName f x) (mangleExp f e)
mangleCmd f (While e c)          = While (mangleExp f e) (mangleCmd f c)
mangleCmd f (Seq cs)             = Seq (map (mangleCmd f) cs)
mangleCmd f (IfThen e c)         = IfThen (mangleExp f e) (mangleCmd f c)
mangleCmd f (IfThenElse e c1 c2) = IfThenElse (mangleExp f e)
                                              (mangleCmd f c1)
                                              (mangleCmd f c2)
mangleCmd f (Return e)           = Return (mangleExp f e)
mangleCmd f Skip                 = Skip

mangleExp :: Name -> Exp -> Exp
mangleExp f (Var x)          = Var $ mangleName f x
mangleExp f (UnaryOp op e)   = UnaryOp op $ mangleExp f e
mangleExp f (BinOp op e1 e2) = BinOp op (mangleExp f e1) (mangleExp f e2)
mangleExp f (FunCall g args) = FunCall g $ map (mangleExp f) args
mangleExp f (ICon i)         = ICon i
mangleExp f (FCon x)         = FCon x
mangleExp f (BCon b)         = BCon b


check :: Either p String -> IO ()
check cond = case cond of
   Left p    -> return ()
   Right msg -> fail msg

translate :: SymbolTable -> Program -> Target
translate st p = undefined


-- 1. To what should we compile Exp?

type ExpTarget = (TupleProgram, RegArg)

-- 2. In which monad do we compile Exp?

type M a = StateT Integer Maybe a

{-
for "free":
get :: M Integer
put :: Integer -> M ()
lift :: Maybe a -> M a
runStateT :: StateT s m a -> s -> m (a, s)
-}

err :: M a
err = lift Nothing

gensym :: M Integer
gensym = do
  i <- get
  put (i+1)
  return i

newReg :: M Register
newReg = do
  i <- gensym
  return (Reg i)

runM :: M a -> a
runM phi = case runStateT phi 0 of
  Just (a,_) -> a
  Nothing    -> error "Something bad happened"

(+>) :: [a] -> a -> [a]
cs +> c = cs ++ [c]

{-
data Exp = Var Name
         | UnaryOp Op Exp
         | BinOp Op Exp Exp
         | FunCall Name [Exp]
         | ICon Integer
         | FCon Double
         | BCon Bool
-}

transExp :: [(Name,(Type,Offset))] -> Exp -> M ExpTarget
transExp st e = case e of
  Var x -> do
    case lookup x st of
      Just (_,o) -> return ([],RegIndOff FP o)
      Nothing    -> lift Nothing
  UnaryOp Neg e -> do
    (c_e,r) <- transExp st e
    rnew <- newReg
    return (c_e +> Negate (RgArg rnew) (RA r), RgArg rnew)
  ICon i -> do
    rnew <- newReg
    return ([Asn (RgArg rnew) (Literal i)], RgArg rnew)
    
  
-- 3. Testing this part of the compiler.

st0 = [("i",(INT,1)),("j",(INT,2))]

e0  = UnaryOp Neg (UnaryOp Neg (ICon 9))
e1  = UnaryOp Neg (UnaryOp Neg (Var "i"))

testExp e = runM (transExp st0 e)

{-
This is pseudo code as yet.
transCmd st (Assign x e) = do pi_e <- transExp st e
                              return "pi_e ; (ASSIGN, FP[o], pi_e)"
  where (t,o) = lookupVar st x
-}

data Target = Target
  
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

lookupFun :: SymbolTable -> Name -> Maybe ([Type],Type)
lookupFun [] f = Nothing
lookupFun (Entry g argtys ty _ : sts) f | f==g      = Just (argtys,ty)
                                        | otherwise = lookupFun sts f

lookupVar :: SymbolTable -> Name -> Maybe (Type,Offset)
lookupVar st x = lkup stsmall x
  where stsmall = concat (map (\ (Entry _ _ _ vs) -> vs) st)

lkup [] x                          = Nothing
lkup ((y,t,o) : sts) x | x==y      = Just (t,o)
                       | otherwise = lkup sts x

type SymbolTable = [STEntry]

mkst :: Program -> SymbolTable
mkst (Program fds) = map mkstFD fds
mkstFD (FunDecl t f args locs _) = Entry f argtys t vs
  where argtys = map (\ (Decl _ t) -> t) args
        vs     = genVBS 0 (args ++ locs)

genVBS o []                = []
genVBS o (Decl x t : dcls) = (x,t,o) : genVBS (o+1) dcls

type Offset  = Integer

data STEntry = Entry Name [Type] Type [(Name,Type,Offset)]

{-

type SymbolTable = [(Type, Name, [Decl], [Decl])]

symboltable :: Program -> SymbolTable
symboltable (Program fds) = map definedNamesFD fds

definedNamesFD :: FunDecl -> (Type, Name, [Decl], [Decl])
definedNamesFD (FunDecl t f params locals _) = (t,f,params,locals)
-}
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

alwaysReturn :: Program -> Either Program ErrorMessage
alwaysReturn p@(Program fds) = case filter noreturn bodies of
  [] -> Left p
  cs -> Right $ "Following commands don't return:" ++ show cs
  where bodies = map (\ (FunDecl _ _ _ _ b) -> b) fds

noreturn :: Cmd -> Bool
noreturn c = case c of
  Return _           -> False
  While _ c          -> True
  IfThen b c         -> True
  IfThenElse b c1 c2 -> noreturn c1 || noreturn c2
  Seq cs             -> or (map noreturn cs)
  _                  -> True
