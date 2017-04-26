module TupleInterp where

import FrontEndTuple
import System.IO
import Control.Exception
import Control.Monad.State
import Data.Char
import Control.Applicative

type Loc    = Integer
type Sto    = (String -> Integer, Loc -> Integer)
type LineNo = Integer

type M a = StateT Sto (StateT Integer (ErrT IO)) a

err :: String -> M a
err msg = lift $ lift $ ErrM $ return $ Error msg

data Err a = OK a | Error String
deOK (OK v)      = v
deOK (Error msg) = error msg
data ErrT m a = ErrM (m (Err a))
deErrM (ErrM phi) = phi

getline :: M LineNo
getline = lift get

inc_line :: M ()
inc_line = do
  l <- getline
  lift (put (l+1))

instance Monad m => Functor (ErrT m) where
  fmap = undefined 

instance Monad m => Applicative (ErrT m) where
  pure  = return
  (<*>) = undefined

instance Monad m => Monad (ErrT m) where
     return = ErrM . return . OK
     (ErrM phi) >>= f  = ErrM (phi >>= \ res ->
                               case res of
                                    OK v -> ((deErrM . f) v)
                                    Error msg -> return (Error msg))

liftErr phi = ErrM (phi >>= (return . OK))

myputStr :: String -> M()
myputStr s = lift $ lift $ liftErr $ putStr s

readNum :: M Integer
readNum = lift $ lift $ liftErr $ readLn

tweek f x v = \ n -> if n==x then v else f n

setreg r v = \ (rf,mem) -> (tweek rf r v,mem)
setloc l v = \ (rf,mem) -> (rf,tweek mem l v)

getreg :: Register -> M Integer
getreg r = do (rf,mem) <- get
              return (rf (show r))
getloc :: Loc -> M Integer
getloc loc = do (rf,mem) <- get
                return (mem loc)

getregarg :: RegArg -> M Integer
getregarg (RgArg r)       = getreg r
getregarg (RegInd r)      = getreg r >>= getloc
getregarg (RegIndOff r o) = do v <- getreg r
                               getloc (v+o)

arg (RA ra)     = getregarg ra
arg (Literal i) = return i


start = do
 putStr "Filename? [type \"q\" to quit] "
 file <- getLine
 if (file=="q") then return ()
   else do h <- try $ openFile file ReadMode
           case h of
                (Left (ErrorCall _)) -> start
                (Right handle)       -> doit handle

doit handle = getit handle >>= \ ir ->
              hClose handle >>
              go ir
                 where go ir = let program = parseTuples (lexer ir)
                               in
                                   run (mng 0 program program)



---
store ra v = case ra of
  (RgArg r)       -> modify (setreg (show r) v)
  (RegInd r)      -> getreg r >>= \ loc -> modify (setloc loc v)
  (RegIndOff r o) -> getreg r >>= \ loc -> modify (setloc (loc+o) v)

tuple :: Tuple -> M ()
tuple (Asn ra a) = do
  i <- arg a
  case ra of
   RgArg r       -> modify (setreg (show r) i)
   RegInd r      -> do loc <- getreg r ;
                       modify (setloc loc i)
   RegIndOff r o -> do loc <- getreg r 
                       modify (setloc (loc+o) i)
tuple (Add ra a1 a2) = do i1 <- arg a1
                          i2 <- arg a2
                          case ra of
                              (RgArg r)       -> modify (setreg (show r) (i1+i2))
                              (RegInd r)      -> do loc <- getreg r
                                                    modify (setloc loc (i1+i2))
                              (RegIndOff r o) -> do loc <- getreg r
                                                    modify (setloc (loc+o) (i1+i2))
tuple (Sub ra a1 a2) = do i1 <- arg a1
                          i2 <- arg a2
                          case ra of
                               RgArg r       -> modify (setreg (show r) (i1-i2))
                               RegInd r      -> do loc <- getreg r
                                                   modify (setloc loc (i1-i2))
                               RegIndOff r o -> do loc <- getreg r
                                                   modify (setloc (loc+o) (i1-i2))
tuple (Div ra a1 a2) = do i1 <- arg a1
                          i2 <- arg a2
                          case ra of
                               RgArg r       -> modify (setreg (show r) (i1 `div` i2))
                               RegInd r      -> do loc <- getreg r
                                                   modify (setloc loc (i1 `div` i2))
                               RegIndOff r o -> do loc <- getreg r
                                                   modify (setloc (loc+o) (i1 `div` i2))
tuple (Mult ra a1 a2) = do i1 <- arg a1
                           i2 <- arg a2
                           case ra of
                                RgArg r       -> modify (setreg (show r) (i1 * i2))
                                RegInd r      -> do loc <- getreg r
                                                    modify (setloc loc (i1 * i2))
                                RegIndOff r o -> do loc <- getreg r
                                                    modify (setloc (loc+o) (i1 * i2))
tuple (Negate ra a)   = do i <- arg a
                           case ra of
                                RgArg r       -> modify (setreg (show r) (-i))
                                RegInd r      -> do loc <- getreg r
                                                    modify (setloc loc (-i))
                                RegIndOff r o -> do loc <- getreg r
                                                    modify (setloc (loc+o) (-i))

tuple (Label i) = return ()
tuple (Read ra) = do myputStr "(stdin) input an integer:"
                     i <- readNum
                     store ra i
tuple (Write a) = do i <- arg a
                     myputStr $ "(stdout) write result: " ++ show i ++ "\n"


--
-- If you experience some form of off-by-one error, look here.
--
mng :: Integer -> [Tuple] -> [Tuple] -> M ()
mng 0 is p = do
  breakpoint is
  myputStr "\nHow many steps [enter=1,to quit=0]? "
  s_i <- lift $ lift $ liftErr getLine
  case s_i of
     "" -> mng 1 is p
     _  -> if i==0 then return () else mng i is p
             where i = if all isDigit s_i then str2int s_i else 1
                   str2int :: String -> Integer
                   str2int str = read str
mng n (Jmp a : is) p         = do label <- arg a
                                  mng (n-1) (findLabel label p) p
mng n (JmpNZ r a : is) p     = do i <- getregarg r
                                  label <- arg a
                                  if i==0 
                                     then
                                       mng (n-1) is p
                                     else
                                       mng (n-1) (findLabel label p) p
mng n (Exit : is) p          = myputStr "Exiting\n"
mng n (i : is) p             = tuple i >> mng (n-1) is p
mng _ [] _                   = myputStr "Exiting\n"

breakpoint is = let next = case is of
                                (c:cs) -> show c
                                []     -> "no next instr."
                in
                do fp <- getreg FP
                   sp <- getreg SP
                   r1 <- getreg (Reg 1)
                   r2 <- getreg (Reg 2)
                   r3 <- getreg (Reg 3)
                   myputStr $ "BREAK: " ++
                              "[FP="++(show fp)++","++
                              "SP="++(show sp)++","++
                              "R1="++(show r1)++","++
                              "R2="++(show r2)++","++
                              "R3="++(show r3)++","++
                              "next instr. = "++ next ++ "]\n"

mytry :: IO a -> M (Either ErrorCall a)
mytry phi = lift (lift (liftErr (try phi)))

rw = (Read (RgArg SP)) : (Write (RA $ RgArg SP)) : rw

{-
test = run (mng 10 rw rw)
-}
run :: M a -> IO a
run phi = (deErrM (runStateT (runStateT phi sto0) lineNo0)) >>= (return . fst . fst . deOK)
    where sto0 = (\ r -> 0, \ l -> 0)
          lineNo0 = 0


findLabel l [] = error $ "Jump to non-existent label " ++ (show l)
findLabel l (Label i : as) = if l==i then (Label i : as) else findLabel l as
findLabel l (_:as)         = findLabel l as
