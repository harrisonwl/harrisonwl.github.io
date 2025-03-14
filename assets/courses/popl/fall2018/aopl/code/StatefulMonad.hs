module StatefulMonad where

import Prelude hiding (LT, GT, EQ, id)
import Base
import Control.Applicative
import Data.Maybe
import Stateful hiding (Stateful, evaluate)

data Stateful t = ST (Memory -> (t, Memory))


instance Functor Stateful where
  fmap f (ST phi) = ST (\ s -> let (t,s) = phi s in (f t,s))

instance Applicative Stateful where
  pure  = return
  (<*>) = undefined

instance Monad Stateful where
  return val = ST (\m -> (val, m))
  (ST c) >>= f = 
    ST (\m -> 
      let (val, m') = c m in
        let ST f' = f val in
          f' m'
      )
        
evaluate :: Exp -> Env -> Stateful Value
-- basic operations
evaluate (Literal v) env = return v
evaluate (Unary op a) env = do
  av <- evaluate a env
  return (unary op av)
evaluate (Binary op a b) env = do
  av <- evaluate a env
  bv <- evaluate b env
  return (binary op av bv)
evaluate (If a b c) env = do
  BoolV cond <- evaluate a env
  evaluate (if cond then b else c) env

-- variables and declarations
evaluate (Declare x e body) env = do    -- non-recursive case
  ev <- evaluate e env
  let newEnv = (x, ev) : env
  evaluate body newEnv
evaluate (Variable x) env = 
  return (fromJust (lookup x env))

-- first-class functions
evaluate (Function x body) env = 
  return (ClosureV  x body env)
evaluate (Call fun arg) env = do
  ClosureV  x body closeEnv <- evaluate fun env
  argv <- evaluate arg env
  let newEnv = (x, argv) : closeEnv
  evaluate body newEnv

-- mutation operations
evaluate (Seq a b) env = do
  evaluate a env
  evaluate b env
evaluate (Mutable e) env = do
  ev <- evaluate e env
  newMemory ev        
evaluate (Access a) env = do
  AddressV i <- evaluate a env
  readMemory i
evaluate (Assign a e) env = do
  AddressV i <- evaluate a env
  ev <- evaluate e env
  updateMemory ev i

newMemory val = ST (\mem-> (AddressV (length mem), mem ++ [val]))

readMemory i = ST (\mem-> (access i mem, mem))

updateMemory val i = ST (\mem-> (val, update i val mem))

runStateful (ST c) = 
   let (val, mem) = c [] in val

execute exp = runStateful (evaluate exp [])


  
