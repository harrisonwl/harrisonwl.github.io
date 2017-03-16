module MonadicConstructions where
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- This is the code from the paper "The Essence of Multitasking" by William Harrison, which
-- was published in the proceedings of 11th International Conference on Algebraic Methodology 
-- and Software Technology (AMAST 2006). You are free to use it as you please, although 
-- should a publication arise from its use, professional ethics requires a citation
-- to the AMAST06 paper.

-- This code is a slight alteration of the original code. It was necessary to accommodate the 
-- tiresome Applicative Monad Proposal (AMP) that has been foisted on GHC users since GHC 7.10
-- was released.

-- In this code, I have chosen to "roll my own" monad transformers rather than use the monad
-- transformer libraries. This choice is more for historical reasons than anything else (I 
-- started hacking monads well before there was a Haskell monad transformer library). There is 
-- an alternative form of the resumption monads available from Hackage which works equally
-- well:   https://hackage.haskell.org/package/monad-resumption

------------------------------------------------------------------
--- The Id monad:
data Id a = Id a
deId (Id x) = x

instance Functor Id where
    fmap = liftM
    
instance Applicative Id where
    pure  = return
    (<*>) = ap

instance Monad Id where
   return v    = Id v
   (Id x) >>= f = f x

data S sto a = S (sto -> (a,sto))
deS (S x) = x

instance Functor (S sto) where
    fmap = liftM
    
instance Applicative (S sto) where
    pure  = return
    (<*>) = ap

instance Monad (S sto) where
   return v      = S $ \ s -> (v,s)
   (S phi) >>= f = S $ \ s0 -> let (v,s1) = phi s0 in deS (f v) s1

g :: S sto sto
g       = S (\ s -> (s,s))

u :: (sto -> sto) -> S sto ()
u delta = S (\ s -> ((), delta s))

--------------- The state monad transformer

data StateT s m a = ST (s -> m (a,s))
deST (ST x) = x

liftSt :: Monad m => m a -> StateT s m a
liftSt phi = ST $ \ s0 -> phi >>= \ v -> return (v,s0)

instance (Monad m) => Functor (StateT s m) where
    fmap = liftM
    
instance (Monad m) => Applicative (StateT s m) where
    pure  = return
    (<*>) = ap

instance Monad m => Monad (StateT s m) where
   return v      = ST (\ s0 -> return (v,s0))
   (ST x) >>= f  = ST (\ s0 -> (x s0) >>= \ (y,s1) -> deST (f y) s1) 

--------------- The resumption monad transformer
data ResT m a = Done a | Pause (m (ResT m a))

instance (Monad m) => Functor (ResT m) where
    fmap = liftM
    
instance (Monad m) => Applicative (ResT m) where
    pure  = return
    (<*>) = ap
                              
instance Monad m => Monad (ResT m) where
    return v       = Done v
    Done v >>= f   = f v
    Pause m >>= f  = Pause (m >>= \r -> return (r >>= f))

-- The "snapshot" resumption monad transformer
data ObsT obs m a = Dn a | Ps obs (m (ObsT obs m a))

instance (Monad m) => Functor (ObsT obs m) where
    fmap = liftM
instance (Monad m) => Applicative (ObsT obs m) where
    pure  = return
    (<*>) = ap
                                         
instance Monad m => Monad (ObsT obs m) where
    return = Dn
    (Dn v) >>= f = f v
    Ps o m >>= f = Ps o (m >>= \r -> return (r >>= f))

{-
We could have used this monad transformer instead of ResT - doing
so would allow us to "instrument" the kernel in ThreadSemantics.hs
without the use of the IO monad.
-}

-- The reactive resumption monad transformer
type Dialog q r a = (q,r->a)
data ReactT q r m a = D a 
                    | P (Dialog q r (m (ReactT q r m a)))

instance (Monad m) => Functor (ReactT q r m) where
    fmap = liftM
instance (Monad m) => Applicative (ReactT q r m) where
    pure  = return
    (<*>) = ap
                                         
instance Monad m => Monad (ReactT q r m) where
    return v      = D v
    D v >>= f     = f v
    P (r,s) >>= f = P (r, \rsp -> (s rsp) >>= \m -> return (m >>= f))  
                                 ---      ^^^"bind" ^^^^^^ "unit" on monad m

