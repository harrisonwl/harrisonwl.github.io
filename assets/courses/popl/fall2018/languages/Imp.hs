module Imp where

import Control.Monad.Identity
import Control.Monad.State

type Loc   = String
data Store = Mem [(Loc,Int)]
initsto    = Mem []

data Imp = Assign Loc Int | Seq Imp Imp
         | Litint Int | Add Imp Imp | Var Loc

c1 = Assign "x" 1
c2 = Assign "x" 2
c3 = Seq c1 c2

-- removes "old" variable bindings 
dropfst x []         = []
dropfst x ((y,v):rs) = if x==y 
                       then dropfst x rs
                       else (y,v) : dropfst x rs

--
-- First Interpreter (leaves out LitInt, Add, and Var)
--

exec :: Imp -> Store -> Store
exec (Assign l i) (Mem s) 
               = Mem ((l,i) : (dropfst l s))
exec (Seq c1 c2) mem0  
               = let 
                     mem1 = exec c1 mem0
                 in 
                     exec c2 mem1

--
-- Second Interpreter (adds LitInt, Add, and Var) and returns two values
--

data Value = NilVal | I Int 

exec2 :: Imp -> Store -> (Value , Store)
exec2 (Assign l i) (Mem s) = (NilVal, Mem ((l,i) : (dropfst l s)))
exec2 (Seq c1 c2) mem0     = let 
                               (_,mem1) = exec2 c1 mem0
                             in 
                               exec2 c2 mem1
exec2 (Litint i) m         = (I i,m)
exec2 (Add i1 i2) m        = let
                               (I v1,m1) = exec2 i1 m
                               (I v2,m2) = exec2 i2 m1
                             in
                               (I (v1 + v2), m2)
exec2 (Var l) m@(Mem s)    = (I v,m)
  where
    Just v = lookup l s

--
-- Third Bonus Interpreter in monadic style (in case you're interested).
--
    
type M = StateT Store Identity 

execM :: Imp -> M Value
execM (Assign l i) = do
                       Mem s <- get
                       put (Mem ((l,i) : (dropfst l s)))
                       return NilVal
execM (Seq c1 c2)  = do
                       execM c1
                       execM c2
execM (Litint i)   = return (I i)
execM (Add i1 i2)  = do
                       I v1 <- execM i1
                       I v2 <- execM i2
                       return (I (v1 + v2))
execM (Var l)      = do
                       Mem s <- get
                       let Just v = lookup l s
                       return (I v)
