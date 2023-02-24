module EssenceOfMultitasking where

import Prelude hiding (exp)
import ThreadLanguage
-- import MonadicConstructions hiding (u,g)
-- ^^^ N.b., not using "roll-your-own" monad transformers

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Resumption
import Control.Monad.Resumption.Reactive

--- Requests & Responses
data Req = Cont | SleepReq | ForkReq Process | BcastReq Message | RecvReq | TestReq
         | ReleaseSemReq | GetSemReq  | PrintReq String | GetPIDReq | ExitReq
         | KillReq PID deriving Show

data Rsp = Ack | RecvRsp Message | GetPIDRsp PID deriving Show

--- In the paper, we "hardwire" the construction of the monads
--- St, R, and Re. Here, we create them using monad transformers.
--- For instrumentation purposes, we start with the 
--- IO monad as base instead of Id as we do in the paper.
type Sto = Loc -> Int
type St  = StateT Sto IO
type R   = ResT St 
type Re  = ReacT Rsp Req St  -- N.b., the order of Rsp and Req

--- I define several new names for 
--- the bind's and unit's of St, R, Re to
--- avoid confusion due to overloading
etaSt :: a -> St a
etaSt = return

(*=) :: St a -> (a -> St b) -> St b
(*=) = (>>=)

(!!=) :: R a -> (a -> R b) -> R b
(!!=) = (>>=)

etaR :: a -> R a
etaR = return -- Done

(||=) :: Re a -> (a -> Re b) -> Re b
(||=) = (>>=)

etaRe :: a -> Re a
etaRe = return -- D

                  ------------------
                  ---  Liftings  ---
                  ------------------

stepR :: Monad m => m a -> ResT m a
stepR x = ResT $ x >>= return . Left

stepRe :: St a -> Re a
stepRe x = ReacT (x >>= return . Left) >>= \ a -> signal Cont >> return a

-- pull is different from the paper. Given an (Re a) thread, 
-- it "executes" its first atom, and returns its continuation.
-- The continuation type is (Either a (Req, Rsp -> Re a)). Being a Left signifies
-- that the thread has completed execution. Being a Right (q,k) signifies that
-- it has made a request q and continuation k awaits the system response (of type Rsp).
pull :: Re a -> R (Either a (Req, Rsp -> Re a))
pull (ReacT x) = ResT (StateT (\ s -> (runStateT x s) >>= \ r ->
                                      case r of
                                         (Left v , s')        -> return (Left (Left v), s')
                                         (Right (q , k) , s') -> return (Right (return (Right (q , k))), s')
                              ))

                  ----------------------------------
                  ---  Extra Structure within St ---
                  ----------------------------------

g :: St Sto
g = StateT (\ u1 -> return (u1,u1))

u :: (Sto -> Sto) -> St ()
u delta = StateT (\u1 -> return ((),delta u1))

getloc :: Loc -> St Int
getloc loc = g >>= \ mem -> return (mem loc)

                  -----------------------------
                  ---  Signals and Handler  ---
                  -----------------------------

sigI :: Req -> Re ()
sigI q = signal q >> return () -- ignores the return Rsp

-------------------------------------------------------------
-- The Semantics of Exp, Event, and Process from Figure 1. --
-------------------------------------------------------------

-- written [i|->v] in the figure
tweek i v sigma = \ n -> if i==n then v else sigma n
store :: Loc -> Int -> Re ()
store loc v = (stepRe . u) (tweek loc v)

-- "exp"  is "E[[-]]" 
exp  :: Exp -> Re Int
exp (Address l)  = lift $ getloc l
exp (Lit i)      = return i
exp GetPID       = signal GetPIDReq ||= (return . prj)
   where prj = \ (GetPIDRsp pid) -> pid

-- "atom" is "A[[-]]"
atom  :: Event -> Re ()
atom (Write x e)    = (exp e) ||= store x
atom Sleep          = sigI SleepReq
atom (Fork p)       = sigI (ForkReq p)
atom (Print m e)    = (exp e) ||= (sigI . PrintReq . output m) 
    where output m v = m ++ " " ++ show v ++ " "
atom (Broadcast e)  = (exp e) ||= (sigI . BcastReq)
atom (Receive x)    = signal RecvReq ||= (store x . prj)
     where prj (RecvRsp m) = m
atom Psem           = sigI GetSemReq
atom Vsem           = sigI ReleaseSemReq
atom (Kill e)       = (exp e) ||= (sigI . KillReq)
atom (Inc l)        = lift (inc l) >> sigI Cont
    where inc l = (getloc l) *= (u . (tweek l) . (\ v -> v+1))

-- "proc" is "P[[-]]"
proc :: Process -> ReacT Rsp Req St b
proc (Process e p) = atom e >> proc p

                  ----------------------
                  ---  Schedulers  ---
                  ----------------------

--
cont :: Monad m => (Rsp -> ReacT Rsp Req m a) -> Rsp -> ReacT Rsp Req m a
-- cont :: (Rsp -> St (Re a)) -> Rsp -> Re a
cont u rsp = ReacT (return (Right (Cont, \ Ack -> u rsp)))
-- ^^^ phase this out

reset :: Monad m => o -> (i -> ReacT i o m a) -> ReacT i o m a
reset q k = ReacT (return (Right (q, k)))

-- The System configuration type:
type Message   = Int
type Semaphore = Int
type PID       = Int
type System    = ([(PID,Re ())],[Message],Semaphore,String,PID)

write :: String -> St ()
write msg = StateT (\ s -> print msg >> return ((),s))

out i m = "proc " ++ show i ++ ": " ++ m

rr :: System ->  R ()
rr ([],_,_,_,_)             = return ()
rr (((pid,t) : w),mq,s,o,g) = pull t >>= \ rt ->
                              case rt of
                                Left _      -> rr (w,mq,s,o,g)
                                Right (q,k) -> handler (w,mq,s,o,g) pid k q

handler :: System -> PID -> (Rsp -> Re ()) -> Req -> R ()
handler (w,mq,s,o,g) i k Cont           = rr (w ++[(i , k Ack)], mq, s, o , g)
handler (w,mq,s,o,g) i k SleepReq       = lift (write msg) >> rr (w ++[(i , k Ack)], mq, s, o , g)
                 where
                       msg  = out i "Sleeping"
handler (w,mq,s,o,g) i k (ForkReq p)    = rr (w', mq, s, o, g+1)
                 where 
                   w'= w ++ [(i, k Ack), (g,proc p)]
handler (w,mq,s,o,g) i k (BcastReq m)   = rr (w++[(i, k Ack)], mq', s, o, g)
                 where
                   mq' = mq ++ [m]
handler (w,[],s,o,g) i k RecvReq        = rr (w',[],s,o,g)
                 where
                   w' = w ++ [(i,reset RecvReq k)]
handler (w,m:mq,s,o,g) i k RecvReq      = rr (w',mq,s,o,g)
                 where 
                   w' = w ++ [(i,k (RecvRsp m))]
handler (w,mq,s,o,g) i k (PrintReq msg) = lift (write msg) >> (rr (w++[(i,k Ack)],mq,s,o++msg,g))
handler (w,mq,s,o,g) i k GetSemReq      = if s>0 then goahead else tryagain
                 where
                   goahead  = rr (w++[(i, k Ack)],mq,s-1,o,g)
                   tryagain = rr (w++[(i, reset GetSemReq k)],mq,s,o,g)
handler (w,mq,s,o,g) i k ReleaseSemReq  = rr (w++[(i,k Ack)],mq,s+1,o,g)
handler (w,mq,s,o,g) i k GetPIDReq = rr (w ++[(i , k (GetPIDRsp i))], mq, s, o , g)
handler (w,mq,s,o,g) i k (KillReq j)    = lift (write msg) >> (rr (w',mq,s,o,g))
                 where
                   w'   = filter exit (w++[(i,k Ack)])
                   exit = \ (i,t) -> i/=j
                   msg  = out i "killing process: " ++ show j

                  ----------------------------
                  ---  Running the System  ---
                  ----------------------------
                  
runprocs :: [Process] -> R ()
runprocs ps = rr (zip ids (map proc ps),[],1,"",lps+1)
    where lps = length ps
          ids = [1..lps]

run :: R a -> St a
run (ResT phi) = phi >>= \ r ->
                 case r of
                   Left v       -> return v
                   (Right phi') -> run phi'
            
takeR :: Int -> R () -> R ()
takeR 0 (ResT phi) = return ()
takeR n (ResT phi) = ResT (phi >>= \ k ->
                           case k of
                             Left _  -> return (Left ())
                             Right x -> return (Right (takeR (n-1) x)))

go :: [Process] -> IO ()
go ps = runStateT (run $ takeR 500 (runprocs ps)) initSto >> return ()
      where initSto = \ n -> 0

                  ----------------------------
                  ---       Examples       ---
                  ----------------------------

-- make a Process which repeats its first argument ad infinitum
loop es []     = loop es es
loop es (e:rs) = Process e (loop es rs)

-- makes a list of n Sleep events
sleep n = map (\ _ -> Sleep) [1..n]

-- inc's x, broadcasts x, and sleeps for 5 cycles.
brc = loop [Inc "x",
            Print "broadcasting " (Address "x"),
            Broadcast (Address "x")] []

-- inc's x, broadcasts x, and sleeps for 5 cycles.
slowbrc = loop body []
   where body = [Inc "x",
                 Print "broadcasting " (Address "x"),
                 Broadcast (Address"x"),
                 Print "sleeping" GetPID
                ] ++  (sleep 5)

-- just receives greedily         
rcv = loop body []
   where body = [Receive "y",Print "  receiving " (Address "y")]

-- try: go on [brc,rcv] and [slowbrc,rcv]

-- try: go [parent]
parent = loop body []
  where body = [Fork child,
                Print "Parent will nap" (Lit 99)
               ] 
               ++ sleep 5 ++
               [Print "Now I'm awake!!!" (Lit 99),
                Receive "noisy",
                Kill (Address "noisy")
               ]
        child = Process (Write "childpid" GetPID)
              $ Process (Broadcast (Address "childpid"))
              $ loop [Print "Weee!!!" (Address "childpid")] []

-- try: go [acqrel,tryacq]
acqrel  = loop body []
  where body = [Psem,Print "Acquired!  pid =" (GetPID)]
               ++ (sleep 5) ++
               [Print "Releasing!  pid =" (GetPID),Vsem] ++ sleep 10

tryacq  = loop body []
   where body = (sleep 2) ++ [Psem,Print "Got it finally!" GetPID]
