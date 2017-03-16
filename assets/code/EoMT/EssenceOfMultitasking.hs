module EssenceOfMultitasking where

import Prelude hiding (exp)
import ThreadLanguage
import MonadicConstructions hiding (u,g)

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
type Re  = ReactT Req Rsp St 

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
etaR = Done

(||=) :: Re a -> (a -> Re b) -> Re b
(||=) = (>>=)

etaRe :: a -> Re a
etaRe = D
                  ------------------
                  ---  Liftings  ---
                  ------------------
rstep :: St a -> R a
rstep x = Pause $ x >>= (return . Done)

step :: St a -> Re a
step x = P (Cont, \ _ -> x >>= (return . D))

                  ----------------------------------
                  ---  Extra Structure within St ---
                  ----------------------------------

g :: St Sto
g = ST (\ u1 -> return (u1,u1))

u :: (Sto -> Sto) -> St ()
u delta = ST (\u1 -> return ((),delta u1))

getloc :: Loc -> St Int
getloc loc = g >>= \ mem -> return (mem loc)

                  -----------------------------
                  ---  Signals and Handler  ---
                  -----------------------------

sig :: Req -> Re Rsp
sig q = P (q, etaSt . etaRe)

sigI :: Req -> Re ()
sigI q = P (q, \ _ -> etaSt $ etaRe ())  


-------------------------------------------------------------
-- The Semantics of Exp, Event, and Process from Figure 1. --
-------------------------------------------------------------

-- written [i|->v] in the figure
tweek i v sigma = \ n -> if i==n then v else sigma n
store loc v = (step . u) (tweek loc v)

-- "exp"  is "E[[-]]" 
exp  :: Exp -> Re Int
exp (Address l)  = step $ getloc l
exp (Lit i)      = return i
exp GetPID       = sig GetPIDReq ||= (return . prj)
    where prj = \ (GetPIDRsp pid) -> pid

-- "atom" is "A[[-]]"
atom  :: Event -> Re ()
atom (Write x e)    = (exp e) ||= store x
atom Sleep          = sigI SleepReq
atom (Fork p)       = sigI (ForkReq p)
atom (Print m e)    = (exp e) ||= (sigI . PrintReq . output m) 
    where output m v = m ++ " " ++ show v ++ " "
atom (Broadcast e)  = (exp e) ||= (sigI . BcastReq)
atom (Receive x)    = sig RecvReq ||= (store x . prj)
    where prj (RecvRsp m) = m
atom Psem           = sigI GetSemReq
atom Vsem           = sigI ReleaseSemReq
atom (Kill e)       = (exp e) ||= (sigI . KillReq)
atom (Inc l)        = step (inc l)
    where inc l = (getloc l) *= (u . (tweek l) . (\ v -> v+1))

-- "proc" is "P[[-]]"
proc (Process e p) = atom e >> proc p

                  ----------------------
                  ---  Schedulers  ---
                  ----------------------

cont :: (Rsp -> St (Re a)) -> Rsp -> Re a
cont u rsp = P (Cont, \ Ack -> u rsp)

-- The System configuration type:
type Message   = Int
type Semaphore = Int
type PID       = Int
type System    = ([(PID,Re ())],[Message],Semaphore,String,PID)

write :: String -> St ()
write msg = ST (\ s -> print msg >> return ((),s))

out i m = "proc " ++ show i ++ ": " ++ m

hand :: System -> (PID,Re ()) -> R ()
hand (w,q,s,o,g) (i,t) =
     case t of
         (D v)       -> rr (w,q,s,o,g)
         (P(Cont,r)) -> rstep (r Ack) >>= next . \ k -> (i,k) 
                 where next t = rr (w++[t],q,s,o,g)
         (P(SleepReq,r)) -> rstep (write msg) >> next
                 where next = rr (w++[(i,P(Cont,r))],q,s,o,g)
                       msg  = out i "Sleeping"


handler :: System -> (PID,Re ()) -> R ()
handler (w,q,s,o,g) (i,t) =
     case t of
         (D v)       -> rr (w,q,s,o,g)
         (P(Cont,r)) -> Pause $ (r Ack) *= \ k -> etaSt $ next (i,k) 
                 where next t = rr (w++[t],q,s,o,g)
         (P(SleepReq,r)) -> Pause $  write msg >> etaSt next
                 where next = rr (w++[(i,P(Cont,r))],q,s,o,g)
                       msg  = out i "Sleeping"
         (P(ForkReq p,r)) -> Pause $ (etaSt next)
                 where parent = (i,cont r Ack)
                       child  = (g,proc p)
                       next   = rr (w++[parent, child],q,s,o,g+1)
         (P(BcastReq m,r)) -> Pause $ etaSt next
                 where q' = q ++ [m]
                       next = rr (w++[(i,cont r Ack)],q',s,o,g)
         (P(RecvReq, r)) | (q==[])  -> Pause $ etaSt next
                 where next = rr (w++[(i,P(RecvReq, r))],[],s,o,g)
         (P(RecvReq, r)) | otherwise -> Pause $ etaSt next
                 where next = rr (w++[(i,cont r (RecvRsp m))],ms,s,o,g)
                       m = head q
                       ms = tail q
         (P(PrintReq msg, r)) -> Pause $ write msg >> etaSt next
                 where next      = rr (w++[(i,P(Cont,r))],q,s,o++msg,g)
         (P(GetSemReq, r))  -> Pause (etaSt next)
                 where next = if s>0 then goahead else tryagain
                       goahead  = rr (w++[(i,P(Cont,r))],q,s-1,o,g)
                       tryagain = rr (w++[(i,P(GetSemReq,r))],q,s,o,g)
         (P(ReleaseSemReq, r)) -> Pause $  etaSt next
                 where next = rr (w++[(i,cont r Ack)],q,s+1,o,g)
         (P(GetPIDReq, r))  -> Pause $ etaSt next
                 where next = rr (w++[(i,cont r (GetPIDRsp i))],q,s,o,g)
         (P(KillReq j, r))  -> Pause $ write msg >> etaSt next
                 where next = rr (wl',q,s,o,g)
                       wl'  = filter (exit j) (w++[(i,cont r Ack)])
                       exit i = \ (pid,t) -> pid/=i
                       msg = out i "killing process: " ++ show j
     
rr :: System ->  R ()
rr ([],_,_,_,_)               = Done ()
rr ((t:w),q,s,o,g) = handler (w,q,s,o,g) t 

                  ----------------------------
                  ---  Running the System  ---
                  ----------------------------
                  
runprocs :: [Process] -> R ()
runprocs ps = rr (zip ids (map proc ps),[],1,"",lps+1)
    where lps = length ps
          ids = [1..lps]

run :: R a -> St a
run (Done v)    = etaSt v
run (Pause phi) = phi *= run

takeR :: Int -> R () -> R ()
takeR 0 _           = Done ()
takeR n (Pause phi) = Pause (phi *= (etaSt . (takeR (n-1))))

go ps = (deST (run $ takeR 500 (runprocs ps))) initSto >> return ()
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
