module CompilerMicroPlus where

import Prelude hiding (EQ,LT,GT)
import MicroPlusAST
import MicroPlusParser
import StaticChecksMicroPlus

import FrontEndTuple
import Control.Monad.Identity
import Data.Maybe
import Control.Monad.State

--
-- Activation records
--

-- SP -> |              |
--       ----------------
--       |    argn      |
--       |    ...       |
--       |    arg1      |
--       ----------------
--       |    FPold     |
--       ----------------
--       | return label |
--       ---------------
-- FP -> |    value     |
--       ---------------

           
type ExpTarget = ([Tuple], RegArg)
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

newReg :: M RegArg
newReg = do
  i <- gensym
  return (RgArg (Reg i))

runM :: M a -> a
runM phi = case runStateT phi 0 of
  Just (a,_) -> a
  Nothing    -> error "Something bad happened"

(+>) :: [a] -> a -> [a]
cs +> c = cs ++ [c]

--
-- "helper functions"
--

returnval :: M ([Tuple],RegArg)
returnval = do
  rv <- newReg
  return ([Mov rv (RA (RegIndOff FP 1))],rv)

inc :: Register -> Tuple
inc reg = Add (RgArg reg) (RA (RgArg reg)) (Literal 1)

dec :: Register -> Tuple
dec reg = Sub (RgArg reg) (RA (RgArg reg)) (Literal 1)

push :: Arg -> [Tuple]
push ra = Mov (RegInd SP) ra :
          inc SP                  : []

pop :: RegArg -> [Tuple]
pop ra = Mov ra (RA (RegInd SP)) :
         dec SP                  : []

storeArgs :: [RegArg] -> [Tuple]
storeArgs []       = []
storeArgs (av:avs) = push (RA av) ++
                     storeArgs avs
  
-- type VarST = [(Name,(Type,Offset))]
-- type FunST = [(Name,([Type],Type,CodeAddress))]
-- mkst :: Program -> (FunST, VarST)

compile :: String -> IO [Tuple]
compile file = do
  p <- file2prog file     -- lexing through parsing
  check (uniquemain p)
  check (alwaysReturn p)
  if noundefined p then return () else fail "Undefined program variables"
  let p' = mangle p
  print p'    
  let (stf,stv) = mkst p'
  let compiled_p = runM $ translate stf stv p'
  return compiled_p

translate :: FunST -> VarST -> Program -> M [Tuple]
translate stf stv (Program fds) = do
  transFDs stf stv fds

transFDs :: FunST -> VarST -> [FunDecl] -> M [Tuple]
transFDs stf stv []       = return []
transFDs stf stv (fd:fds) = do
  c_fd <- transFD stf stv fd
  c_fds <- transFDs stf stv fds
  return $ c_fd ++ c_fds

transFD :: FunST -> VarST -> FunDecl -> M [Tuple]
transFD stf stv (FunDecl _ f _ _ body) = do
  let (_,_,lf) = fromJust (lookup f stf)
  c_body <- transCmd stf stv body
  return $ Label lf : c_body

transArgs :: FunST -> VarST -> [Exp] -> M [([Tuple],RegArg)]
transArgs stf stv []     = return []
transArgs stf stv (e:es) = do
  c_r_e <- transExp stf stv e
  ets   <- transArgs stf stv es
  return (c_r_e : ets)
    
transSeq :: FunST -> VarST -> [Cmd] -> M [Tuple]
transSeq stf stv []     = return []
transSeq stf stv (c:cs) = do
  codec  <- transCmd stf stv c
  codecs <- transSeq stf stv cs
  return $ codec ++ codecs

transExp :: FunST -> VarST -> Exp -> M ([Tuple],RegArg)
transExp stf stv e = case e of
  Var x -> do
    case lookup x stv of
      Just (_,o) -> return ([],RegIndOff FP o)
      Nothing    -> err

  UnaryOp Neg e -> do
    (c_e,r) <- transExp stf stv e
    rnew <- newReg
    return (c_e +> Negate rnew (RA r), rnew)

  ICon i -> do
    rnew <- newReg
    return ([Mov rnew (Literal i)], rnew)

  BCon b         -> do
    rnew <- newReg
    let i = if b then 1 else 0 -- True(False) encoded by 1(0).
    return ([Mov rnew (Literal i)], rnew)

  BinOp EQ e1 e2 -> do
    (ce1,r1) <- transExp stf stv e1
    (ce2,r2) <- transExp stf stv e2
    r_new    <- newReg
    l_exit   <- gensym
    l_false  <- gensym
    let c_eq = ce1 ++
               ce2 ++
              [Sub r_new (RA r1) (RA r2),
               JmpNZ r_new (Literal l_false),
               Mov r_new (Literal 1), -- i.e., true
               Jmp (Literal l_exit),
               Label l_false,
               Mov r_new (Literal 0), -- i.e., false
               Label l_exit
              ]
    return (c_eq,r_new)

  BinOp Minus e1 e2 -> do
    (ce1,r1) <- transExp stf stv e1
    (ce2,r2) <- transExp stf stv e2
    r_new    <- newReg
    let c_eq = ce1 ++
               ce2 +>
               Sub r_new (RA r1) (RA r2)
    return (c_eq,r_new)

  BinOp Times e1 e2 -> do
    (ce1,r1) <- transExp stf stv e1
    (ce2,r2) <- transExp stf stv e2
    r_new    <- newReg
    let c_eq = ce1 ++
               ce2 +>
               Mult r_new (RA r1) (RA r2)
    return (c_eq,r_new)

  FunCall f es   -> do
    args         <- transArgs stf stv es
    return_label <- gensym
    rv           <- newReg
    let (ts,t,l)     = fromJust (lookup f stf)
                           -- the code for each argument        
    let compute_args = concat $ map fst args           
                           -- locations of the argument values
    let c_rhs        = map snd args
                           -- space for return value        
    let calling_code =  Mov (RgArg FP) (RA (RgArg SP)) :
                       (push (Literal 0)            ++
                           -- push return label                        
                        push (Literal return_label) ++
                           -- store old FP                        
                        push (RA (RgArg FP))        ++
                           -- code to eval args                        
                        compute_args                ++
                           -- store arg values                        
                        storeArgs c_rhs             +>
                           -- actually call the function                        
                        Call (Literal l)            +>
                           -- code to which we return                        
                        Label return_label)         ++
                           -- store the return value; pop stack                       
                        pop rv                         
    return (calling_code, rv)

transCmd :: FunST -> VarST -> Cmd -> M TupleProgram
transCmd stf stv c = case c of
  Assign x e -> do
    (ce,re) <- transExp stf stv e
    case lookup x stv of
      Just (_,o) -> return $ ce +> (Mov (RegIndOff FP o) (RA re))

  Seq cs             -> transSeq stf stv cs

  IfThen e c         -> do
    (c_e,r_e) <- transExp stf stv e
    c_c       <- transCmd stf stv c
    rtst      <- newReg
    l_exit    <- gensym
    return $ c_e ++
            [Sub rtst (Literal 1) (RA r_e),
             JmpNZ rtst (Literal l_exit)] ++
             c_c           ++
            [Label l_exit]

  IfThenElse e c1 c2 -> do
    (c_e,rv) <- transExp stf stv e
    code_c1  <- transCmd stf stv c1
    code_c2  <- transCmd stf stv c2
    l1       <- gensym
    lexit    <- gensym
    return $ (c_e +>
              JmpNZ rv (Literal l1)) ++
              code_c2 ++
             [Jmp (Literal lexit),
              Label l1] ++
              code_c1   ++
             [Label lexit]

  While e c          -> do
    (c_e,r_e) <- transExp stf stv e
    code_c    <- transCmd stf stv c
    rtst      <- newReg
    l_exit    <- gensym
    loop      <- gensym
    let code = [Label loop] ++
                c_e ++
               [Sub rtst (Literal 1) (RA r_e),
                JmpNZ rtst (Literal l_exit)] ++
                code_c           ++
               [Jmp (Literal loop),
                Label l_exit]
    return code          

  Return e           -> do
    (c_e,r_e) <- transExp stf stv e
    return $ c_e                                        +>
             Mov (RegInd FP) (RA r_e)                   +>
             Add (RgArg SP) (RA (RgArg FP)) (Literal 1) +>
             Mov (RgArg FP) (RA (RegIndOff FP 2))       +>
             Ret
             
  Skip               -> return []



--
-- Testing
--

testCmd :: Cmd -> TupleProgram
testCmd c = runM (transCmd stf0 stv0 c)

c0 = IfThen e2 (Assign "i" (ICon 101))
c1 = Return (ICon 120)

stv0 = [("i",(INT,1)),("j",(INT,2))]
stf0 = [("fac",([INT],INT,1001))]

e0  = UnaryOp Neg (UnaryOp Neg (ICon 9))
e1  = UnaryOp Neg (UnaryOp Neg (Var "i"))
e2  = BinOp EQ (UnaryOp Neg (Var "i")) (ICon 99)
e3  = FunCall "fac" [ICon 5]

testExp e = runM (transExp stf0 stv0 e)

{-
--
-- Code for fac(5)
--
   (ASSIGN,M[SP],0),
   (ADD,SP,SP,1),
   (ASSIGN,M[SP],1),
   (ADD,SP,SP,1),
   (ASSIGN,M[SP],FP),
   (ADD,SP,SP,1),
   (ASSIGN,R0,5),
   (ASSIGN,M[SP],R0),
   (ADD,SP,SP,1),
   (CALL,1001),
   (LABEL,1),
   (ASSIGN,R2,M[SP]),
   (SUB,SP,SP,1)]       ,R2)

--
-- Code for return 120
--
   (ASSIGN,R0,120),
   (ASSIGN,M[FP],R0),
   (ASSIGN,FP,M[FP+2]),
   (ADD,SP,FP,1)

-}
