module CompilerMicroPlus where

import Prelude hiding (EQ,LT,GT)
import MicroPlusAST
import MicroPlusParser
import StaticChecksMicroPlus

import FrontEndTuple
import Control.Monad.Identity
import Control.Monad.State

compile file = do
  p <- file2prog file     -- lexing through parsing
  check (uniquemain p)
  check (alwaysReturn p)
  if noundefined p then return () else fail "Undefined program variables"
  let p' = mangle p
  print p'    
  let syms = mkst p'
  return (translate syms p)

translate :: SymbolTable -> Program -> [Tuple]
translate st p = undefined

-- 1. To what should we compile Exp?

type ExpTarget = ([Tuple], RegArg)

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


transExp :: [(Name,(Type,Offset))] -> Exp -> M ExpTarget
transExp st e = case e of
  Var x -> do
    case lookup x st of
      Just (_,o) -> return ([],RegIndOff FP o)
      Nothing    -> err
  UnaryOp Neg e -> do
    (c_e,r) <- transExp st e
    rnew <- newReg
    return (c_e +> Negate (RgArg rnew) (RA r), RgArg rnew)
  ICon i -> do
    rnew <- newReg
    return ([Mov (RgArg rnew) (Literal i)], RgArg rnew)

  BinOp op e1 e2 -> undefined
  FunCall f es   -> undefined
  FCon d         -> undefined
  BCon b         -> undefined
    
  
-- 3. Testing this part of the compiler.

st0 = [("i",(INT,1)),("j",(INT,2))]

e0  = UnaryOp Neg (UnaryOp Neg (ICon 9))
e1  = UnaryOp Neg (UnaryOp Neg (Var "i"))

testExp e = runM (transExp st0 e)

-- 4. What about compiling Cmd?

transCmd :: [(Name,(Type,Offset))] -> Cmd -> M TupleProgram
transCmd st c = case c of
  Assign x e -> do
    (ce,re) <- transExp st e
    case lookup x st of
      Just (_,o) -> return $ ce +> (Mov (RegIndOff FP o) (RA re))
    
