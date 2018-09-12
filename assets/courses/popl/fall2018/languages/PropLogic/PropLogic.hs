module PropLogic where

type Var = String

data Prop = Atom Var 
          | Not Prop
          | Imply Prop Prop
            
{-            
Recall the derivation of ( - p ):
   Prop ->
     ( - Prop )
     ( - p )

We can represent this as the following Haskell value:
-}

negp = Not (Atom "p")

instance Show Prop where
  show (Atom p)            = p
  show (Not prop)          = "(-" ++ show prop ++ ")"
  show (Imply prop1 prop2) = "(" ++ show prop1 ++ " => " ++ show prop2 ++ ")"

instance Eq Prop where
  (Atom p) == (Atom q)       = p == q
  (Not x) == (Not y)         = x == y
  (Imply x y) == (Imply u v) = (x == u) && (y == v)
  _ == _                     = False
  
--   
-- Connectives  
--  
  
orPL :: Prop -> Prop -> Prop
orPL phi gamma  = Imply (Not phi) gamma
andPL phi gamma = Not (orPL (Not phi) (Not gamma))
iffPL phi gamma = andPL (Imply phi gamma) (Imply gamma phi)
  
--
-- Axioms
--

-- ...as functions

axiom1 :: Prop -> Prop -> Prop
axiom1 phi gamma     = Imply phi (Imply gamma phi)

axiom2 :: Prop -> Prop -> Prop -> Prop
axiom2 phi gamma psi = Imply pre post
   where pre  = Imply phi (Imply gamma psi)
         post = Imply
                   (Imply phi gamma)
                   (Imply phi psi)

axiom3 phi gamma = Imply pre post
   where pre  = Imply (Not gamma) (Not phi)
         post = Imply hyp gamma
            where hyp  = Imply (Not gamma) phi

-- ...as data type

data Axiom = Ax1 Prop Prop
           | Ax2 Prop Prop Prop
           | Ax3 Prop Prop
             deriving Eq
                       
instance Show Axiom where
  show (Ax1 phi gamma)     = show (axiom1 phi gamma)
  show (Ax2 phi gamma psi) = show (axiom2 phi gamma psi)
  show (Ax3 phi gamma)     = show (axiom3 phi gamma)

data Theorem = AxiomInst Axiom | ModusPonens Theorem Theorem Prop

instance Show Theorem where
  show (AxiomInst ax)             = show ax
  show (ModusPonens x y z) 
     =         show x ++ "   " ++ show y
       ++ "\n--------------------------------\n    " ++
                       show z

-- Subproof of A => A

a = Atom "A"

subproof = ModusPonens hyp1 hyp2 conc
  where hyp1 = AxiomInst (Ax1 a (Imply a a))
        hyp2 = AxiomInst (Ax2 a (Imply a a) a)
        conc = Imply (Imply a (Imply a a))
                     (Imply a a)

